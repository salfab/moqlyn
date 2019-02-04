using System;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Moqlyn
{
    using System.Collections.Generic;
    using System.Diagnostics;

    using Microsoft.CodeAnalysis.Editing;
    using Microsoft.CodeAnalysis.FindSymbols;

    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(MoqlynCodeFixProvider)), Shared]
    public class MoqlynCodeFixProvider : CodeFixProvider
    {
        private const string title = "Make uppercase";

        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            get
            {
                return ImmutableArray.Create(MoqlynAnalyzer.DiagnosticId);
            }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/FixAllProvider.md for more information on Fix All Providers
            return WellKnownFixAllProviders.BatchFixer;
        }

        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Let's assume we only have 1 fix to show for the diagnostic.
            var diagnostic = context.Diagnostics.Single();
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            var objectCreation = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf()
                .OfType<ObjectCreationExpressionSyntax>().First();
            root = root.TrackNodes(objectCreation);
            var constructors =
                ((INamedTypeSymbol)context.Document.GetSemanticModelAsync().Result.GetSymbolInfo(objectCreation.Type)
                        .Symbol).Constructors.OrderByDescending(o => o.Parameters.Length);

            var fixTitle = diagnostic.Descriptor.Description.ToString();

            // Find the type declaration identified by the diagnostic.


            foreach (var constructor in constructors)
            {
                // Register a code action that will invoke the fix.
                context.RegisterCodeFix(
                    CodeAction.Create(
                        title: fixTitle,
                        createChangedDocument: c => this.CompleteConstructorWithMocks(
                            context.Document,
                            objectCreation,
                            c,
                            constructor,
                            root),
                        equivalenceKey: fixTitle),
                    diagnostic);
            }

        }

        private async Task<Document> CompleteConstructorWithMocks(
            Document document,
            ObjectCreationExpressionSyntax constructorToComplete,
            CancellationToken cancellationToken,
            IMethodSymbol constructor,
            SyntaxNode rootNode)
        {
            var passedArguments = constructorToComplete.ArgumentList.Arguments;
            var parameters = constructor.Parameters;

            if (passedArguments.Count(o => !o.IsMissing) == parameters.Length)
            {
                return document;
            }

            var compilation = await document.Project.GetCompilationAsync(cancellationToken);

            // Handle MockRepository

            // The handler will be a pass-through by default.
            Func<SyntaxNode, Task<SyntaxNode>> mockRepositoryRootNodeHandler = null;

            bool mockRepositorySymbolExists = false;

            var mockRepositorySymbolName = "MockRepository";
            mockRepositorySymbolExists = this.CompilationLookUpSymbols(
                constructorToComplete,
                mockRepositorySymbolName,
                compilation);

            if (!mockRepositorySymbolExists)
            {
                mockRepositorySymbolName = "mockRepository";
                mockRepositorySymbolExists = this.CompilationLookUpSymbols(
                    constructorToComplete,
                    mockRepositorySymbolName,
                    compilation);
            }

            if (!mockRepositorySymbolExists)
            {
                // we use the handlers mechanism, because even if the mockRepository doesn't exist, we aren't sure yet if we're going to need it.
                // This is why we will only call the handler conditionnally, later on.
                mockRepositoryRootNodeHandler = async rn =>
                    {                        
                        var mockRepositoryAsVariable = await this.CreateAndInitializeMockRepositoryAsVariable(document, mockRepositorySymbolName, constructorToComplete);

                        // insert declaration / instantiation of mock repository before we do anything with it.
                        return rn.InsertNodesBefore(
                            rn
                                .GetCurrentNode(constructorToComplete)
                                .FirstAncestorOrSelf<ExpressionStatementSyntax>(),
                            new[] { mockRepositoryAsVariable });
                    };
            }

            // Handle .ctor arguments
            for (int i = 0; i < parameters.Length; i++)
            {
                // create argument to pass
                var parameter = parameters[i];
                ArgumentSyntax node;
                if (parameter.Type.IsAbstract)
                {
                    if (!mockRepositorySymbolExists)
                    {
                        Debug.Assert(mockRepositoryRootNodeHandler != null, $"there's a bug here: " + nameof(mockRepositoryRootNodeHandler) +" can't be null");

                        rootNode = await mockRepositoryRootNodeHandler(rootNode);

                        mockRepositorySymbolExists = true;
                    }

                    // TODO: add support for settings to create properties instead of variables : useful for MSpec and inherited contexts.
                    // Note: this implementation is variable-specific, because the declaration and assignment will be done in one line. Properties can't do that.
                    var mockSymbolAsVariable = this.CreateAndInitializeInjectedMockSymbolAsVariable(
                        parameter,
                        document,
                        mockRepositorySymbolName);

                    rootNode = await this.InsertMockSymbolAsVariableInDocumentAsync(
                                                          mockSymbolAsVariable,
                                                          constructorToComplete,
                                                          document,
                                                          cancellationToken,
                                                          rootNode);

                    // TODO: If we delegate the creation of the syntax to a dedicated service :
                    // the next line will also be in the responsibility of that service, since the implementation is specific to lcoal variables (not compatible with Properties)
                    var name = ((LocalDeclarationStatementSyntax)mockSymbolAsVariable).Declaration.Variables.Single()
                        .Identifier.Value;

                    // TODO: add configuration to use .Moq() to get the mock instead of using .Object to get the object.
                    node = SyntaxFactory.Argument(SyntaxFactory.IdentifierName(name + ".Object"));
                }
                else
                {
                    node = SyntaxFactory.Argument(SyntaxFactory.IdentifierName("TODO"));
                }

                passedArguments = passedArguments.Insert(i, node);
            }

            var argumentsList = SyntaxFactory.ArgumentList(passedArguments);
            var updatedObjectCreation = constructorToComplete.WithArgumentList(argumentsList);

            var currentObjectCreation = rootNode.GetCurrentNode(constructorToComplete);

            var updatedSyntaxTree = rootNode.ReplaceNode(currentObjectCreation, updatedObjectCreation);

            return document.WithSyntaxRoot(updatedSyntaxTree);
        }



        private async Task<SyntaxNode> InsertMockSymbolAsVariableInDocumentAsync(
            SyntaxNode mockSymbolAsVariable,
            SyntaxNode objectCreationUsingMocks,
            Document document,
            CancellationToken cancellationToken,
            SyntaxNode rootNode)
        {
            // the declaration of the mocks must be placed before the objectCreationUsingMocks.


            return rootNode.InsertNodesBefore(
                rootNode.GetCurrentNode(objectCreationUsingMocks).FirstAncestorOrSelf<ExpressionStatementSyntax>(),
                new[] { mockSymbolAsVariable });
        }

        private SyntaxNode CreateAndInitializeInjectedMockSymbolAsVariable(
            IParameterSymbol parameter,
            Document document,
            string mockRepositorySymbolName)
        {
            return this.CreateMockedArgumentSymbolAsVariable(
                "injected{0}Mock",
                Casing.PascalCase,
                parameter,
                document,
                mockRepositorySymbolName);
        }


        /// <summary>Creates the mocked argument symbol as variable.</summary>
        /// <param name="namingFormat">The naming format.</param>
        /// <param name="parameterNameCasing">The casing to apply to the parameter name when applying the <param name="namingFormat"></param>.</param>
        /// <param name="parameter">The parameter.</param>
        /// <param name="document"></param>
        /// <param name="mockRepositorySymbolName"></param>
        /// <returns>The symbol of the declaration for the mock object.</returns>
        /// <exception cref="ArgumentOutOfRangeException">parameterNameCasing - null</exception>
        private SyntaxNode CreateMockedArgumentSymbolAsVariable(
            string namingFormat,
            Casing parameterNameCasing,
            IParameterSymbol parameter,
            Document document,
            string mockRepositorySymbolName)
        {
            string parameterName;
            switch (parameterNameCasing)
            {
                case Casing.PascalCase:
                    parameterName = parameter.Name.Substring(0, 1).ToUpperInvariant() + parameter.Name.Substring(1);
                    break;
                case Casing.AsIs:
                    parameterName = parameter.Name;
                    break;
                default:
                    throw new ArgumentOutOfRangeException(nameof(parameterNameCasing), parameterNameCasing, null);
            }

            var typeSymbol = document.Project.GetCompilationAsync().Result.GetTypeByMetadataName("Moq.Mock`1")
                .Construct(parameter.Type);

            var name = string.Format(namingFormat, parameterName);
            var syntaxGenerator = SyntaxGenerator.GetGenerator(document);
            var type = syntaxGenerator.TypeExpression(typeSymbol, true);

            // TODO: Add support for different of initialization approaches: loose mocks (ugh), new statements, and alternatie naming of the MockRepository (camel-casing ?)
            SyntaxNode initializer = SyntaxFactory.InvocationExpression(
                SyntaxFactory.MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.IdentifierName(mockRepositorySymbolName),
                    SyntaxFactory.GenericName(SyntaxFactory.Identifier("Create")).WithTypeArgumentList(
                        SyntaxFactory.TypeArgumentList(
                            SyntaxFactory.SingletonSeparatedList(
                                SyntaxFactory.ParseTypeName(
                                    parameter.CanBeReferencedByName
                                        ? parameter.Type.Name
                                        : parameter.Type.ToDisplayString()))))).NormalizeWhitespace());

            return syntaxGenerator.LocalDeclarationStatement(type, name, initializer);
        }

        private async Task<SyntaxNode> CreateAndInitializeMockRepositoryAsVariable(
            Document document,
            string mockRepositorySymbolName,
            ObjectCreationExpressionSyntax constructorToComplete)
        {
            var compilationPromise = document.Project.GetCompilationAsync();
            var syntaxGenerator = SyntaxGenerator.GetGenerator(document);
            var compilation = await compilationPromise;

            var moqBehaviorSymbol = compilation.GetTypeByMetadataName("Moq.MockBehavior");
            var moqBehaviorSyntax = (TypeSyntax)syntaxGenerator.TypeExpression(moqBehaviorSymbol, true);
            var moqBehaviorExpresion = SyntaxFactory.MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression, 
                moqBehaviorSyntax,
                SyntaxFactory.Token(SyntaxKind.DotToken),                                
                SyntaxFactory.IdentifierName("Strict"));
            var moqBehaviorArgumentsList = SyntaxFactory.ArgumentList(SyntaxFactory.SeparatedList(new[] { SyntaxFactory.Argument(moqBehaviorExpresion) }));

            var mockRepositoryTypeSymbol = compilation.GetTypeByMetadataName("Moq.MockRepository");
            var mockRepositoryTypeSyntax = (TypeSyntax)syntaxGenerator.TypeExpression(mockRepositoryTypeSymbol, true);

            var initializer = SyntaxFactory.ObjectCreationExpression(mockRepositoryTypeSyntax).WithArgumentList(moqBehaviorArgumentsList);
            var indentation = constructorToComplete.AncestorsAndSelf().OfType<ExpressionStatementSyntax>().First().GetLeadingTrivia().Last(o => o.IsKind(SyntaxKind.WhitespaceTrivia));

            return syntaxGenerator
                .LocalDeclarationStatement(mockRepositoryTypeSyntax, mockRepositorySymbolName, initializer)
                .NormalizeWhitespace()
                .WithLeadingTrivia(SyntaxTriviaList.Create(indentation));
        }

        // Or use the SemanticModel from the CSharpCompilation.
        // Possibly slower? Also, not as much fun as manually traversing trees.
        public bool CompilationLookUpSymbols(SyntaxNode currentNode, string symbolToFind, Compilation compilation)
        {
            var tree = currentNode
                .Ancestors()
                .First(n => n is MethodDeclarationSyntax || n is ConstructorDeclarationSyntax).SyntaxTree;

            var model = compilation.GetSemanticModel(tree);
            return model
                .LookupSymbols(currentNode.SpanStart, name: symbolToFind)
                .Any(o => o.Kind == SymbolKind.Property);
        }
    }

    enum Casing
    {        
        PascalCase, 
        AsIs
    }
}

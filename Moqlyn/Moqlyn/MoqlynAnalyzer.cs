using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace Moqlyn
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class MoqlynAnalyzer : DiagnosticAnalyzer
    {
        public const string DiagnosticId = "Moqlyn";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString CompleteCtorDescription = new LocalizableResourceString(nameof(Resources.CtorNeedsArguments), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Moq-Helper";

        private static DiagnosticDescriptor CompleteCtorDiagnosticDescriptor = new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: CompleteCtorDescription);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(CompleteCtorDiagnosticDescriptor); } }

        public override void Initialize(AnalysisContext context)
        {
            // TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            // context.RegisterSymbolAction(AnalyzeSymbol, SymbolKind.NamedType);
            context.RegisterSyntaxNodeAction(this.AnalyzeConstructorsSyntaxNode, SyntaxKind.ObjectCreationExpression);            
        }

        private void AnalyzeConstructorsSyntaxNode(SyntaxNodeAnalysisContext context)
        {
            if (context.Node.Kind() == SyntaxKind.ObjectCreationExpression)
            {
                var node = (ObjectCreationExpressionSyntax)context.Node;

                if (node.ArgumentList == null)
                {
                    // the object is using an object initializer instead of a .ctor : we are not interested.
                    return;
                }

                var namedTypeSymbol = context.SemanticModel.GetSymbolInfo(((ObjectCreationExpressionSyntax)context.Node).Type).Symbol as INamedTypeSymbol;

                var constructors = namedTypeSymbol
                    .Constructors
                    .OrderByDescending(o => o.Parameters.Length);


                if (constructors.All(o => o.Parameters.Length != node.ArgumentList.Arguments.Count(a => !a.IsMissing)))
                {
                    var diagnostic = Diagnostic.Create(CompleteCtorDiagnosticDescriptor, node.GetLocation(), node, constructors);

                    context.ReportDiagnostic(diagnostic);
                }
            }

            if (context.Node.Kind() == SyntaxKind.OpenParenToken)
            {
                
            }

            if (context.Node.Kind() == SyntaxKind.ArgumentList)
            {
                var node = (ArgumentListSyntax)context.Node;

                // handle argument list syntax node.

                var diagnostic = Diagnostic.Create(CompleteCtorDiagnosticDescriptor, node.GetLocation(), node);

                context.ReportDiagnostic(diagnostic);
            }
            else if (context.Node.Kind() == SyntaxKind.InvocationExpression)
            {

                var node = (InvocationExpressionSyntax)context.Node;
                // handle invocation expression syntax
                if (node.Parent.Kind() == SyntaxKind.SimpleLambdaExpression &&
                    node.Parent.Parent.Parent.Kind() == SyntaxKind.ArgumentList &&
                    node.Parent.Parent.Parent.Parent.ChildNodes().Count() == 2 &&
                    node.Parent.Parent.Parent.Parent.ChildNodes().First().Kind() == SyntaxKind.SimpleMemberAccessExpression &&
                    node.Parent.Parent.Parent.Parent.ChildNodes().First().GetLastToken().ValueText == "Setup" &&
                    context.SemanticModel.GetDiagnostics(node.GetLocation().SourceSpan).Any())
                {

                    var diagnostic = Diagnostic.Create(CompleteCtorDiagnosticDescriptor, node.GetLocation(), node);

                    context.ReportDiagnostic(diagnostic);
                }
                // if node.Parent.Kind() == SimpleLambdaExpression
                // if node.Parent.Parent.Parent.Kind() == ArgumentList
                // if node.Parent.Parent.Parent.Parent.ChildNodes().Count == 2
                // if node.Parent.Parent.Parent.Parent.ChildNodes().First()
                // if node.Parent.Parent.Parent.Parent.ChildNodes().First().Kind() == SimpleMemberAccessExpression
                // node.Parent.Parent.Parent.Parent.ChildNodes().First().GetLastToken().Value == "Setup"

                //if (node.ArgumentList.Arguments.Count == 0 && )
            }
        }

//        private static void AnalyzeSymbol(SymbolAnalysisContext context)
//        {
//            // TODO: Replace the following code with your own analysis, generating Diagnostic objects for any issues you find
//            var namedTypeSymbol = (INamedTypeSymbol)context.Symbol;

//            if (!namedTypeSymbol.IsType)
//            {
//                return;
//            }

//            var constructors = namedTypeSymbol.Constructors.OrderByDescending(o =>o.Parameters.Length);
            
////            context.Compilation.SyntaxTrees.First().GetRootAsync()
//            // Find just those named type symbols with names containing lowercase letters.
//            if (namedTypeSymbol.Name.ToCharArray().Any(char.IsLower))
//            {
//                // For all such symbols, produce a diagnostic.
//                var diagnostic = Diagnostic.Create(Rule, namedTypeSymbol.Locations[0], namedTypeSymbol.Name);

//                context.ReportDiagnostic(diagnostic);
//            }
//        }
    }
}

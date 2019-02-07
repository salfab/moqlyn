# Moqlyn

## What is it ?

Moqlyn is a Roslyn analyzer + code-fixer that will improve the way you write unit tests by generating the mocks you inject in your _Subject Under Test_.

We plan on extending it in the future, with more tedious tasks that we might want to automate.
Think moq's setup methods auto-completed with ``It.IsAny<T>()``, or having an analyzer that enforces that every method that has been setup is also verified, or it raises a warning !

## Limitations
First version with support for 1 constructo and no support for customization.

typical scenario :
- moq's ``MockRepository`` is an existing and instantiated property (PascalCase) or variable (camelCase).
    - If a ``MockRepository`` is not found, a local variable is created.
- tests are written in a flat-style (as opposed to context/specification)
- mocks are named injectedXxxMock
- mocks are declared as local variables, not properties.
- mocked objects are accessed with the .Object property.

These points are subject to further customization and flexibility in the future.

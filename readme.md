# Moqlyn

First version with support for 1 constructor, 1 mockable argument, and no support for customization.

typical scenario :
- moq's MockRepository is a property (PascalCase)
- tests are written in a flat-style (as opposed to context/specification)
- mocks are named injectedXxxMock
- mocks are declared as local variables, not properties.
- mocked objects are accessed with the .Object property.

these points are subject to further customization and flexibility in the future.

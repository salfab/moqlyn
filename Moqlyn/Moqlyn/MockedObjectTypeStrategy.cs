namespace Moqlyn
{
    internal enum MockedObjectTypeStrategy
    {
        /// <summary>
        /// The generated symbol will be of type Mock<T>, and will be named injectedXxxMock. The actual mocked object can be accessed through the Mock<T>.Object property.
        /// </summary>
        MockOfT,

        /// <summary>
        /// The generated symbol will be of the type of the object to mock; it will be named injectedXxx. The Mock<T> object can be accessed via the Mock.Get(injectedXxx) static method.
        /// </summary>
        TypeOfObject
    }
}
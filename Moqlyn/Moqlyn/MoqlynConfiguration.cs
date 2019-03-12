using System;

namespace Moqlyn
{
    public class MoqlynConfiguration
    {
        private static readonly Lazy<MoqlynConfiguration> DefaultMoqlynConfiguration = new Lazy<MoqlynConfiguration>(() => new MoqlynConfiguration
                                                                                                         {
                                                                                                             MockedObjectTypeStrategy = MockedObjectTypeStrategy.TypeOfObject
                                                                                                         });

        public MockedObjectTypeStrategy MockedObjectTypeStrategy { get; set; }

        public static MoqlynConfiguration Default => DefaultMoqlynConfiguration.Value;
    }
}
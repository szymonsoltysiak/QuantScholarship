module Configuration

type ConfigKey = string
type ConfigValue = string
type ConfigEntry = { Key : ConfigKey; Value : ConfigValue }
type ConfigCategory = { Category : string; Config : ConfigEntry[]  }
type JsonConfig = ConfigCategory []

type ConfigurationRecord = 
    {
        Key : ConfigKey
        Value : ConfigValue
    }

type Configuration = Map<ConfigKey, ConfigValue>
type MarketData = Map<ConfigKey, ConfigValue>
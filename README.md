The subscanr R package facilitates the extraction of Polkadot ecosystem blockchain data from [Subscap.io](https://www.subscan.io/) using their [api](https://docs.api.subscan.io).  You can get a free (or paid) api key [here](https://forms.gle/YegvB8S2VGhASMQi6).  The free api allows you 5 calls per second, which seems adequate for most users.

The Subscan api supports 48 different endpoints, such as Polkadot, Kusama, Darwinia, Acala Mandala, Karura, etc.

*WARNING: The [subscanr](https://github.com/rogerjbos/subscanr) R package is still alpha and hasn't been fully tested.  Also, not all the available api calls have been implemented yet.*

In particular, according to [the Subscan riot forum](https://riot.im/app/#/room/#subscan:matrix.org), the price feed is not yet available for the Karura endpoint.

You can install the package as follows:

```
library(devtools)
install_github("rogerjbos/subscanr")
```

Then set your private api key as follows:

```
subscan_api_key <- '2a3xxxxxxxxxxxxxxxxxxxxxxxxxxxx9'

```
If you don't yet have an api key, you can set it to blank and Subscan will use your IP address.

```
subscan_api_key <- ''

```

The vignette folder has an example of pulling in some DEX Swaps data from the Karura endpoint.  This example also uses the [cryptor](https://github.com/rogerjbos/cryptor) R package to get pricing from [CoinGecko](https://www.coingecko.com/en) since the Subscan pricing is not available yet.

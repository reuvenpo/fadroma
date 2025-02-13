export * from '@fadroma/scrt'

import { Console, bold } from '@fadroma/ops'

const console = Console('@fadroma/scrt-1.2')

import { dirname, fileURLToPath } from '@fadroma/scrt'
export const __dirname = dirname(fileURLToPath(import.meta.url))

import { ScrtDockerBuilder, resolve } from '@fadroma/scrt'
export const buildImage      = 'hackbg/fadroma-scrt-builder:1.2'
export const buildDockerfile = resolve(__dirname, 'Scrt_1_2_Build.Dockerfile')
export class ScrtDockerBuilder_1_2 extends ScrtDockerBuilder {
  buildImage      = buildImage
  buildDockerfile = buildDockerfile
}

import { ScrtContract, Client } from '@fadroma/scrt'
export abstract class ScrtContract_1_2<C extends Client> extends ScrtContract<C> {
  Builder = ScrtDockerBuilder_1_2
}

const { FADROMA_PREPARE_MULTISIG } = process.env
import { Scrt, ScrtAgentTX } from '@fadroma/scrt'
export class Scrt_1_2 extends Scrt {
  Agent = FADROMA_PREPARE_MULTISIG ? ScrtAgentTX : ScrtAgentJS_1_2
}

import { Identity, Agent, ScrtAgentJS } from '@fadroma/scrt'
export class ScrtAgentJS_1_2 extends ScrtAgentJS {

  API = PatchedSigningCosmWasmClient_1_2

  static create (options: Identity): Promise<Agent> {
    return ScrtAgentJS.createSub(ScrtAgentJS_1_2, options)
  }
  async upload (pathToBinary: string) {
    const result = await super.upload(pathToBinary)
    // Non-blocking broadcast mode returns code ID = -1,
    // so we need to find the code ID manually from the output
    if (result.codeId === -1) {
      try {
        for (const log of result.logs) {
          for (const event of log.events) {
            for (const attribute of event.attributes) {
              if (attribute.key === 'code_id') {
                Object.assign(result, { codeId: Number(attribute.value) })
                break
              }
            }
          }
        }
      } catch (e) {
        console.warn(`Could not get code ID for ${bold(pathToBinary)}: ${e.message}`)
        console.debug(`Result of upload transaction:`, result)
        throw e
      }
    }
    return result
  }
}

const { SCRT_API_URL, SCRT_AGENT_NAME, SCRT_AGENT_ADDRESS, SCRT_AGENT_MNEMONIC, DATAHUB_KEY } = process.env
import { URL } from 'url'
export class Scrt_1_2_Mainnet extends Scrt_1_2 {
  id         = 'secret-4'
  isMainnet  = true
  apiURL     = new URL(SCRT_API_URL||`https://secret-4--lcd--full.datahub.figment.io/apikey/${DATAHUB_KEY}/`)
  defaultIdentity = {
    name:     SCRT_AGENT_NAME,
    address:  SCRT_AGENT_ADDRESS,
    mnemonic: SCRT_AGENT_MNEMONIC
  }
  constructor () {
    super()
    this.setDirs()
  }
}
export class Scrt_1_2_Testnet extends Scrt_1_2 {
  id         = 'pulsar-2'
  isTestnet  = true
  apiURL     = new URL(SCRT_API_URL||`https://secret-pulsar-2--lcd--full.datahub.figment.io/apikey/${DATAHUB_KEY}/`)
  defaultIdentity = {
    name:     SCRT_AGENT_NAME,
    address:  SCRT_AGENT_ADDRESS,
    mnemonic: SCRT_AGENT_MNEMONIC
  }
  constructor () {
    super()
    this.setDirs()
  }
}
export class Scrt_1_2_Localnet extends Scrt_1_2 {
  id         = 'fadroma-scrt-12'
  node       = new DockerScrtNode_1_2()
  isLocalnet = true
  apiURL     = new URL('http://localhost:1337')
  defaultIdentity = 'ADMIN'
  constructor () {
    super()
    this.setNode()
    this.setDirs()
  }
}
import { DockerScrtNode, ChainNodeOptions, TextFile } from '@fadroma/scrt'
export class DockerScrtNode_1_2 extends DockerScrtNode {
  readonly chainId: string = 'fadroma-scrt-12'
  readonly image:   string = "enigmampc/secret-network-sw-dev:v1.2.0"
  readonly readyPhrase     = 'indexed block'
  readonly initScript      = new TextFile(__dirname, 'Scrt_1_2_Init.sh')
  constructor ({
    image,
    chainId,
    identities,
    stateRoot
  }: ChainNodeOptions = {}) {
    super()
    if (image)      this.image = image
    if (chainId)    this.chainId = chainId
    if (identities) this.identitiesToCreate = identities
    this.setDirectories(stateRoot)
  }
}

export default {
  Agent:    ScrtAgentJS_1_2,
  Builder:  ScrtDockerBuilder_1_2,
  Contract: ScrtContract_1_2,
  Chains: {
    /** Create an instance that runs a node in a local Docker container
     *  and talks to it via SecretJS */
    'localnet-1.2': () => new Scrt_1_2_Localnet(),
    /** Create an instance that talks to to pulsar-1 testnet via SecretJS */
    'pulsar-2':     () => new Scrt_1_2_Testnet(),
    /** Create an instance that talks to to the Secret Network mainnet via secretcli */
    'secret-4':     () => new Scrt_1_2_Mainnet()
  },
  Node: DockerScrtNode_1_2,
}

import { SigningCosmWasmClient, BroadcastMode } from 'secretjs'
export class PatchedSigningCosmWasmClient_1_2 extends SigningCosmWasmClient {
  submitRetries      = 10
  resubmitDelay      = 1000
  blockQueryInterval = 1000
  resultRetries      = 10
  resultRetryDelay   = 2000
  async postTx (tx: any): Promise<any> {
    // Only override for non-default broadcast modes
    if (this.restClient.broadcastMode === BroadcastMode.Block) {
      console.info('Broadcast mode is block, bypassing patch')
      return super.postTx(tx)
    }
    let submitRetries = this.submitRetries
    while (submitRetries--) {
      // 1. Submit the transaction
      const sent = (await this.getBlock()).header.height
      const {transactionHash: id} = await super.postTx(tx)
      // 2. Poll for block height to increment
      await this.waitForNextBlock(sent)
      // 3. Start querying for the full result.
      try {
        return await this.getTxResult(id)
      } catch (error) {
        if (error.rethrow) {
          // 4. If the transaction resulted in an error, rethrow it so it can be decrypted
          console.warn(`Transaction ${bold(id)} returned error:\n${error.message}`)
          throw error
        } else {
          // 5. If the transaction simply hasn't committed yet, query for the result again.
          console.info(`Submit TX: ${bold(submitRetries)} retries left...`)
          await new Promise(ok=>setTimeout(ok, this.resubmitDelay))
        }
      }
    }
  }
  async waitForNextBlock (sent: number) {
    while (true) {
      await new Promise(ok=>setTimeout(ok, this.blockQueryInterval))
      const now = (await this.getBlock()).header.height
      if (now > sent) break
    }
  }

  async instantiate (...args: Array<any>) {
    let {transactionHash:id} = await super.instantiate(...args)
    return await this.getTxResult(id)
  }

  async execute (...args: Array<any>) {
    let {transactionHash:id} = await super.execute(...args)
    return await this.getTxResult(id)
  }

  async getTxResult (id: string) {
    let resultRetries = this.resultRetries
    while (resultRetries--) {
      try {
        //console.info(`Requesting result of tx ${id}`)
        const result = await this.restClient.get(`/txs/${id}`)
        // if result contains error, throw it so it can be decrypted
        const {raw_log, logs = []} = result as any
        if (raw_log.includes('failed') || raw_log.includes('out of gas')) {
          console.warn(`Transaction ${bold(id)} failed`)
          const error = new Error(raw_log)
          Object.assign(error, { rethrow: true })
          throw new Error(raw_log)
        }
        Object.assign(result, { transactionHash: id, logs })
        return result
      } catch (error) {
        // retry only on 404, throw all other errors to decrypt them
        if (!error.message.includes('404')) {
          Object.assign(error, { rethrow: true })
          throw error
        }
        if (process.env.FADROMA_PRINT_TXS) {
          //console.warn(error.message)
          //console.info(`Requesting result of ${id}: ${resultRetries} retries left`)
        }
        await new Promise(ok=>setTimeout(ok, this.resultRetryDelay))
        continue
      }
    }
  }
}

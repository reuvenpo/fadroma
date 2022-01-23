export * from '@fadroma/ops'

export * from './ScrtAgentCLI'
export * from './ScrtAgentJS'

export * from './ScrtChainAPI'
export * from './ScrtChainNode'
export * from './ScrtContract'
export * from './ScrtGas'

import { open } from '@hackbg/tools'
export function openFaucet () {
  const url = `https://faucet.secrettestnet.io/`
  console.debug(`Opening ${url}...`)
  open(url)
}

import { resolve, dirname, fileURLToPath } from '@hackbg/tools'

const __dirname = dirname(fileURLToPath(import.meta.url))

export const buildScript = resolve(__dirname, 'ScrtBuild.sh')

import type { IChain, IAgent } from '@fadroma/ops'
import { init as _init, MigrationOptions } from '@fadroma/ops'
import { CHAINS } from './ScrtChainAPI'
export type Context = { chain: IChain, admin: IAgent }
export async function init (chainName: string, options?: MigrationOptions): Promise<Context> {
  return _init(CHAINS, chainName, options)
}

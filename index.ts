import type { IChainConnectOptions } from '@fadroma/ops'
export type Chains = Record<string, (options: IChainConnectOptions)=>IChain>
export const CHAINS: Chains = {}

import Scrt_1_0 from '@fadroma/scrt-1.0'
Object.assign(CHAINS, Scrt_1_0.Chains)
export { Scrt_1_0 }

import Scrt_1_2 from '@fadroma/scrt-1.2'
Object.assign(CHAINS, Scrt_1_2.Chains)
export { Scrt_1_2 }

import { fileURLToPath } from 'url'
import runCommands from '@hackbg/komandi'
import type { IChain, IAgent } from '@fadroma/ops'
export type MigrationContext = { chain: IChain, admin: IAgent }
export type Command<T> = (MigrationContext)=>Promise<T>
export type Commands = Record<string, Command<any>>
import { init } from '@fadroma/ops'
export class Fadroma {

  chains = CHAINS

  chainId = process.env.FADROMA_CHAIN

  commands: Commands = {}

  command <T> (name: string, command: Command<T>) {
    this.commands[name] = () => this.run(command)
  }

  async run <T> (command: Command<T>): Promise<T> {
    if (!this.chainId) {
      console.log('Please set your FADROMA_CHAIN environment variable to one of the following:')
      console.log('  '+Object.keys(this.chains).join('\n  '))
      // TODO if interactive, display a selector which exports it for the session
      process.exit(1)
    }
    const { chain, admin } = await init(this.chains, this.chainId)
    return await command({ chain, admin })
  }

  module (url: string): Commands {
    // if main
    if (process.argv[1] === fileURLToPath(url)) {
      runCommands.default(
        this.commands,
        process.argv.slice(2)
      )
    }
    // if imported
    return this.commands
  }

}

export default new Fadroma()
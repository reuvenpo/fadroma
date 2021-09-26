import type { Agent } from './Agent'
import type { ContractInitOptions } from './Contract'
import { ContractUpload } from './ContractUpload'
import { JSONDirectory } from '@fadroma/tools'
import { backOff } from 'exponential-backoff'

export abstract class ContractInit extends ContractUpload {
  init: {
    prefix?:  string
    agent?:   Agent
    address?: string
    label?:   string
    msg?:     any
    tx?: {
      contractAddress: string
      data:            string
      logs:            Array<any>
      transactionHash: string
    }
  } = {}

  constructor (options: ContractInitOptions = {}) {
    super(options)
    if (options.prefix)  this.init.prefix  = options.prefix
    if (options.label)   this.init.label   = options.label
    if (options.agent)   this.init.agent   = options.agent
    if (options.address) this.init.address = options.address
    if (options.initMsg) this.init.msg     = options.initMsg }

  /** The agent that initialized this instance of the contract. */
  get instantiator () { return this.init.agent }
  /** The on-chain address of this contract instance */
  get address () { return this.init.address }
  /** A reference to the contract in the format that ICC callbacks expect. */
  get link () { return { address: this.address, code_hash: this.codeHash } }
  /** A reference to the contract as an array */
  get linkPair () { return [ this.address, this.codeHash ] as [string, string] }
  /** The on-chain label of this contract instance.
    * The chain requires these to be unique.
    * If a prefix is set, it is appended to the label. */
  get label () { return this.init.prefix
    ? `${this.init.prefix}/${this.init.label}`
    : this.init.label }
  /** The message that was used to initialize this instance. */
  get initMsg () { return this.init.msg }
  /** The response from the init transaction. */
  get initTx () { return this.init.tx }
  /** The full result of the init transaction. */
  get initReceipt () {
    return { label:    this.label
           , codeId:   this.codeId
           , codeHash: this.codeHash
           , initTx:   this.initTx } }

  private initBackoffOptions = {
    retry (error: any, attempt: number) {
      if (error.message.includes('500')) {
        console.warn(`Error 500, retry #${attempt}...`)
        return true }
      else {
        return false } } }

  private initBackoff (fn: ()=>Promise<any>) {
    return backOff(fn, this.initBackoffOptions) }

  async instantiate (agent?: Agent) {
    if (!this.address) {
      if (agent) this.init.agent = agent
      if (!this.codeId) throw new Error('Contract must be uploaded before instantiating')
      this.init.tx = await this.initBackoff(()=>
        this.instantiator.instantiate(this.codeId, this.label, this.initMsg))
      this.init.address = this.initTx.contractAddress
      this.save() }
    else if (this.address) {
      throw new Error(`This contract has already been instantiated at ${this.address}`) }
    return this.initTx }

  /** Used by Ensemble to save multiple instantiation receipts in a subdir. */
  setPrefix (prefix: string) {
    this.init.prefix = prefix
    return this }

  /** Save the contract's instantiation receipt in the instances directory for this chain.
    * If prefix is set, creates subdir grouping contracts with the same prefix. */
  save () {
    let dir = this.init.agent.chain.instances
    if (this.init.prefix) dir = dir.subdir(this.init.prefix, JSONDirectory).make()
    dir.save(this.init.label, this.initReceipt)
    return this } }
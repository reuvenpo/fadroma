export function expose (object: Object) {
  onmessage = async ({ data }) => {
    const [id, [method, ...args], observe] = data
    if (typeof object[method] !== 'function') {
      console.warn(`${object.constructor?.name}: Tried to call ${method} which is not a method`, data)
    } else {
      if (observe) {
        Promise.resolve(object[method](...args)).then(observable=>observable.subscribe({
          next  (x: any) { postMessage([id, null, x]) },
          error (e: any) { postMessage([id, e, undefined]) },
          complete ()    { postMessage([id, null, undefined, true]) }
        }))
      } else {
        try {
          const returned = await Promise.resolve(object[method](...args))
          postMessage([id, null, returned, true])
        } catch (error) {
          postMessage([id, error, undefined, true])
        }
      }
    }
  }
}

export { isWorkerRuntime } from './Common'

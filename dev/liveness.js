// Liveness evidence: the shadow test runner normally calls process.exit;
// with it neutralised, Node must exit on its own — which only happens when
// no server / socket / ref'd timer survives the suite.
const realExit = process.exit;
process.exit = (code) => { console.log('[liveness] runner asked to exit with', code, '- ignored; waiting for the event loop to drain'); };
const t0 = Date.now();
process.on('exit', () => {
  console.log('[liveness] process exited by itself after', Date.now() - t0, 'ms; active resources at exit:', JSON.stringify(process.getActiveResourcesInfo()));
});
const watchdog = setTimeout(() => { console.log('[liveness] FAIL: still alive 15 s after start; active resources:', JSON.stringify(process.getActiveResourcesInfo())); realExit(2); }, 15000);
watchdog.unref();
require('/home/borret/parts/express-cljs/target/node-test.js');

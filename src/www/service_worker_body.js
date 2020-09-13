// These variables prefixed by build:
// var files = ['main.bc.js', ...]
// var version = '<digest>'

function log(msg) {
	console.log("app_cache["+version+"]: "+ msg)
}
function cache() {
	return caches.open(version)
}

self.addEventListener("install", function(event) {
	log("install")
	self.skipWaiting()
	event.waitUntil(
		cache()
			.then(function(cache) {
				return cache.addAll(files)
			})
			.then(function() {
				log("install completed")
			})
	)
})

self.addEventListener("fetch", function(event) {
	if (event.request.method !== 'GET') {
		return
	}
	log('fetch: ' + event.request.url)
	var response = cache().then(function(cache) {
		return cache.match(event.request).then(function(match) {
			log("match for " + event.request.url + " = " + match)
			if (match) {
				return match
			} else {
				log("Falling back to fetch")
				return fetch(event.request)
			}
		})
	})
	event.respondWith(response)
})

self.addEventListener("activate", function(event) {
	log("activate")
	event.waitUntil(
		caches.keys().then(function (keys) {
			return Promise.all(
				keys.filter(function (key) {
					return key != version
				}).map(function (key) {
					log("deleting cache for " + key)
					return caches.delete(key)
				})
			)
		}).then(function() {
			log('activate complete')
		})
  )
})




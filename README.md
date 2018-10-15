# Gerbil NetVis

This is a simple network visualizer for pubsub message propagation using [cairo](https://github.com/vyzo/gerbil-cairo); see [gerbil-simsub](https://github.com/vyzo/gerbil-simsub) for the network simulator driving the visualization.

# Usage

```
(import :vyzo/simsub/scripts :vyzo/netvis)

;; capture random seed for reproducible graphs
(def seed (random-source-state-ref default-random-source))
(simple-floodsub-simulation messages: 1 fanout: 1  transcript: (save-transcript-to-file "/tmp/floodsub.out"))

(random-source-state-set! default-random-source seed)
(simple-gossipsub-simulation messages: 1 fanout: 1 transcript: (save-transcript-to-file "/tmp/gossipsub.out"))

(animate-floodsub! "/tmp/floodsub.out" 100)
(animate-gossipsub! "/tmp/gossipsub.out" 100)
```

# License

MIT; © vyzo 2018

(define-module (stay-alive extensions)
               #:export (dijkstra
                          find-paths
                          see-level))

(load-extension "libguile_dijkstra" "init_dijkstra")
(load-extension "libguile_fov" "init_fov")

#include <libguile.h>

typedef struct point {
  int x;
  int y;
} Point;

typedef struct weighted_point {
  unsigned int weight;
  Point point;
  Point prev;
  unsigned short int index;
} WeightedPoint;

typedef int LeastPathCompare(void *, void *);
typedef void LeastPathCallback(void *, int);

int cmp_weighted_points(void * v1, void * v2) {
  WeightedPoint * point1 = (WeightedPoint*)v1;
  WeightedPoint * point2 = (WeightedPoint*)v2;
  if(point1 -> weight < point2 -> weight) return -1;
  else if(point1 -> weight > point2 -> weight) return 1;
  else return 0;
}


void weighted_point_callback(void * item, int index) {
  WeightedPoint * point = (WeightedPoint *)item;
  point -> index = index;
}

void percolateUp(int size, void ** points, int index, LeastPathCompare * cmp, LeastPathCallback * callback) {
  while(index > 1 && cmp(points[index], points[index/2]) < 0) {
    void * t = points[index / 2];
    points[index/2] = points[index];
    callback(points[index / 2], index / 2);
    points[index] = t;
    callback(points[index], index);
    index /= 2;
  }
}

void percolateDown(int size, void ** points,  int hole, LeastPathCompare * cmp, LeastPathCallback * callback) {
  int child;
  void * tmp = points[hole];
  for(; hole * 2 <= size; hole = child) {
    child = hole * 2;
    if(child != size && cmp(points[child + 1], points[child]) < 0) {
      child ++;
    }
    if(cmp(points[child], tmp) < 0) {
      void * t;
      t = points[hole];
      points[hole] = points[child];
      callback(points[hole], hole);
      points[child] = t;
      callback(points[child], child);
    }
    else {
      break;
    }
  }
  points[hole] = tmp;
  callback(points[hole], hole);
}

void init_heap(int size, void ** points, LeastPathCompare * cmp, LeastPathCallback * callback) {
  int i;
  for(i = size / 2; i > 0; i--) {
    percolateDown(size, points, i, cmp, callback);
  }
}

void * deleteMin(int size, void ** points, LeastPathCompare * cmp, LeastPathCallback * callback) {
  void * target = points[1];
  points[1] = points[size];
  callback(points[size], 1);
  percolateDown(size - 1, points, 1, cmp, callback);
  return target;
}

void percolateEitherWay(int size, void ** points, int index, LeastPathCompare * cmp, LeastPathCallback * callback) {
  if(index > 1 && cmp(points[index], points[index/2]) < 0) {
    percolateUp(size, points, index, cmp, callback);
  }
  else {
    percolateDown(size, points, index, cmp, callback);
  }
}

void insert(int size, void ** points, void * point, LeastPathCompare * cmp, LeastPathCallback * callback) {
  int hole = size + 1;
  points[hole] = point;
  callback(points[hole], hole);
  for(; hole > 1 && cmp(points[hole], points[hole/2]) < 0; hole /=2) {
      void * t = points[hole];
      points[hole] = points[hole/2];
      callback(points[hole], hole);
      points[hole/2] = t;
      callback(points[hole/2], hole/2);
  }
}

void explore_path(int x, int y, int size, int cols, WeightedPoint * u, int ** visited, WeightedPoint ** paths, WeightedPoint ** distances, int * weights, int extra_weight) {
  if(!visited[y][x]) {
    int alt = u->weight + weights[(y * cols) + x] + extra_weight;
    if(alt < paths[y][x].weight) {
      paths[y][x].weight = alt;
      percolateEitherWay(size, (void**)distances, paths[y][x].index, cmp_weighted_points, weighted_point_callback);
      paths[y][x].prev.x = u->point.x;
      paths[y][x].prev.y = u->point.y;
    }
  }
}

#define UNVISITED_SQUARE (INT_MAX/2)
void find_paths(WeightedPoint ** paths, Point start, int cut_corners, int * weights, int rows, int cols) {
  WeightedPoint * distances[rows * cols];
  int ** visited;
  int size = 0;
  visited = calloc(rows, sizeof(int*));
  int r;
  for(r = 0; r < rows; r++) {
    visited[r] = calloc(cols, sizeof(int));
    int c;
    for(c = 0; c < cols; c++) {
      distances[size + 1] = &paths[r][c];
      distances[size + 1] -> point = (Point){c, r};
      if(c > 0 && c < cols - 1 && r > 0 && r < rows - 1) {
        distances[size + 1] -> index = size + 1;
        if(r == start.y && c == start.x) { distances[size + 1] -> weight = 0; }
        else { distances[size + 1] -> weight = UNVISITED_SQUARE; }
        size++;
        visited[r][c] = 0;
      }
      else {
        visited[r][c] = 1;
      }
      paths[r][c].prev.x = 0;
      paths[r][c].prev.y = 0;
    }
  }
  init_heap(size, (void**)distances, cmp_weighted_points, weighted_point_callback);
  while(size) {
    WeightedPoint * u = deleteMin(size--, (void**)distances, cmp_weighted_points, weighted_point_callback);
    explore_path(u->point.x - 1, u->point.y, size, cols, u, visited, paths, distances, weights, 0);
    explore_path(u->point.x + 1, u->point.y, size, cols, u, visited, paths, distances, weights, 0);
    explore_path(u->point.x, u->point.y - 1, size, cols, u, visited, paths, distances, weights, 0);
    explore_path(u->point.x, u->point.y + 1, size, cols, u, visited, paths, distances, weights, 0);
    if(cut_corners) {
      explore_path(u->point.x - 1, u->point.y + 1, size, cols, u, visited, paths, distances, weights, 1);
      explore_path(u->point.x + 1, u->point.y - 1, size, cols, u, visited, paths, distances, weights, 1);
      explore_path(u->point.x - 1, u->point.y - 1, size, cols, u, visited, paths, distances, weights, 1);
      explore_path(u->point.x + 1, u->point.y + 1, size, cols, u, visited, paths, distances, weights, 1);
    }
    visited[u->point.y][u->point.x] = 1;
  }
  for(r = 0; r < rows; r++) {
    free(visited[r]);
  }
  free(visited);
}

SCM dijkstra(SCM scm_weights, SCM scm_start, SCM scm_cut_corners_p) {
  int row = scm_to_int(SCM_CAR(scm_start));
  int col = scm_to_int(SCM_CAR(SCM_CDR(scm_start)));
  SCM dimensions = scm_array_dimensions(scm_weights);
  int rows = scm_to_int(SCM_CAR(dimensions));
  int cols = scm_to_int(SCM_CAR(SCM_CDR(dimensions)));

  int cut_corners_p = scm_to_bool(scm_cut_corners_p);
  int * weights = calloc(rows * cols, sizeof(int *));
  WeightedPoint ** weighted_paths = calloc(rows, sizeof(WeightedPoint *));

  scm_t_array_handle weights_handle;
  scm_array_get_handle(scm_weights, &weights_handle);

  int i_row, i_col;
  for(i_row = 0; i_row < rows; i_row++) {
    weighted_paths[i_row] = calloc(cols, sizeof(WeightedPoint));
    for(i_col = 0; i_col < cols; i_col++) {
      ssize_t pos = scm_array_handle_pos(&weights_handle, scm_list_2(scm_from_int(i_row), scm_from_int(i_col)));
      weights[i_row * cols + i_col] = scm_to_int(scm_array_handle_ref(&weights_handle, pos));
    }
  }

  scm_array_handle_release(&weights_handle);
  find_paths(weighted_paths, (Point){col, row}, cut_corners_p, weights, rows, cols);

  SCM scm_paths = scm_make_array(scm_from_int(0), dimensions);
  for(i_row = 0; i_row < rows; i_row++) {
    for(i_col = 0; i_col < cols; i_col++) {
      scm_array_set_x(scm_paths,
		      scm_list_2(scm_from_int(weighted_paths[i_row][i_col].prev.y), scm_from_int(weighted_paths[i_row][i_col] .prev.x)),
		      scm_list_2(scm_from_int(i_row), scm_from_int(i_col)));
    }
    free(weighted_paths[i_row]);
  }

  free(weighted_paths);
  free(weights);

  return scm_paths;
}

void init_dijkstra() {
  scm_c_define_gsubr("dijkstra", 3, 0, 0, dijkstra);
}

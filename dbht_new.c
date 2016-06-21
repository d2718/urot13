
/* dbht_new.c
 *
 * a "binary hash tree" implemented as an array instead of nodes
 *
 * last update: 2016-06-20
 *
 * REQUIRES
 * <stdlib.h>
 * <stdio.h>
 */

#include <stdlib.h>
#include <stdio.h>

/* This is where we can define
 * DBHT_DEBUG
 * DBHT_TEST
 * if we need to do either of those things.
 */

typedef struct dbht_cell {
  unsigned int key;
  const unsigned int * value;
} dbht_Cell;

typedef struct dbht {
  unsigned int size;
  dbht_Cell * data;
} DBHT;

/* Considering the rot13 application, I think doing anything else would
 * be unexpected.
 */
void die_on_error(const char const * msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

/* It turns out this is the only error message we might encounter. */
const char * const dbht_OM_ERROR = "Unable to allocate memory.";

/* Return an empty DBHT with enough initial memory to hold new_size
 * dbht_Cells. The returned DBHT's .data pointer will need to be manually
 * free()d.
 */
DBHT dbht_create(unsigned int new_size) {
  DBHT tree;
  tree.size = new_size;
  tree.data = malloc(sizeof(dbht_Cell) * new_size);
  if(tree.data == NULL)
    die_on_error(dbht_OM_ERROR);
  for(unsigned int n = 0; n < new_size; ++n)
    tree.data[n] = (dbht_Cell) { 0, NULL };
  return tree;
}

/* Return the index of the given child for the "node" at index i.
 */
unsigned int dbht_l_child_index(unsigned int i) {
  return 2 * i + 1;
}
unsigned int dbht_r_child_index(unsigned int i) {
  return 2 * i + 2;
}

/* Double the size of the array of dbht_Cells. Mainly used internally
 * by functions that insert and populate.
 */
void dbht_grow(DBHT * tree) {
  unsigned int old_size, new_size, n;
  dbht_Cell * new_data;
  old_size = tree->size;
  new_size = old_size * 2;
  new_data = malloc(sizeof(dbht_Cell) * new_size);
  if(new_data == NULL)
    die_on_error(dbht_OM_ERROR);
  for(n = 0; n < old_size; ++n)
    new_data[n] = tree->data[n];
  for(n = old_size; n < new_size; ++n)
    new_data[n] = (dbht_Cell) { 0, NULL };
  free(tree->data);
  tree->data = new_data;
  tree->size = new_size;
}

/* Insert a key: value pair into the tree. Does not balance.
 */
void dbht_insert(DBHT * tree,
                 unsigned int nkey,
                 unsigned int * nvalue) {
  unsigned int idx = 0;
  char cont = 1;
  dbht_Cell * data = tree->data;

  while(cont) {
    if(idx >= tree->size) {
      dbht_grow(tree);
    } else if(data[idx].value == NULL) {
      data[idx].key = nkey;
      data[idx].value = nvalue;
      cont = 0;
    } else if(nkey < data[idx].key) {
      idx = dbht_l_child_index(idx);
    } else if(nkey > data[idx].key) {
      idx = dbht_r_child_index(idx);
    } else if(nkey == data[idx].key) {
      data[idx].key = nkey;
      data[idx].value = nvalue;
      cont = 0;
    } else {
      die_on_error("dbht_insert(): fell through loop; shouldn't happen.");
    }
  }
  return;
}

/* Return the height of a subtree with its root at the given index.
 */
unsigned int dbht_height_from_index(const DBHT const * tree,
                                    unsigned int index) {

  if(index >= tree->size) {
    return 0;
  } else if(tree->data[index].value == NULL) {
    return 0;
  } else {
    unsigned int l_height = dbht_height_from_index(tree, dbht_l_child_index(index));
    unsigned int r_height = dbht_height_from_index(tree, dbht_r_child_index(index));
    if(l_height > r_height)
      return l_height + 1;
    else
      return r_height + 1;
  }
}

/* Return the "bias" of a subtree with its root at the given index.
 * Bias is right_height - left_height, so negative biases mean the left branch
 * is higher.
 */
int dbht_bias_from_index(const DBHT const * tree,
                         unsigned int index) {
  if(index >= tree->size)
    return 0;
  else if(tree->data[index].value == NULL)
    return 0;
  else
    return ((int) dbht_height_from_index(tree, dbht_r_child_index(index)) -
            (int) dbht_height_from_index(tree, dbht_l_child_index(index)));
}

/* Sort an array of dbht_Cells by their keys. Uses a not-totally-in-place
 * quicksort, but leaves the result in place.
 */
void quicksort_Cell_array(dbht_Cell * arr, unsigned int siz) {
  if(siz < 2) {
    /* do nothing */
  } else if(siz == 2) {
    if(arr[0].key > arr[1].key) {
      dbht_Cell temp = arr[0];
      arr[0] = arr[1];
      arr[1] = temp;
    }
  } else {
    unsigned int pi = siz/2;
    unsigned int pivot = arr[siz/2].key;
    unsigned int i_hi = siz-1;
    unsigned int i_lo = 0;
    unsigned int n;
    dbht_Cell * temp;
    temp = malloc(sizeof(dbht_Cell) * siz);
    if(temp == NULL)
      die_on_error(dbht_OM_ERROR);

    for(n = 0; n < pi; ++n) {
      if(arr[n].key < pivot)
        temp[i_lo++] = arr[n];
      else
        temp[i_hi--] = arr[n];
    }
    for(n = pi+1; n < siz; ++n) {
      if(arr[n].key < pivot)
        temp[i_lo++] = arr[n];
      else
        temp[i_hi--] = arr[n];
    }
    temp[i_lo] = arr[pi];
    for(n = 0; n < siz; ++n)
      arr[n] = temp[n];
    free(temp);

    quicksort_Cell_array(arr, i_lo);
    quicksort_Cell_array(&arr[i_lo+1], siz-(i_lo+1));
  }
  return;
}

/* Turn a sorted array (or sub-array) of cells (src) into a tree
 * (or subtree), with its root node at the given tgt_idx.
 */
void dbht_entree(dbht_Cell * src, DBHT * tgt,
                 unsigned int src_start,
                 unsigned int src_end,
                 unsigned int tgt_idx) {
  unsigned int width = src_end - src_start;

  if(tgt_idx >= tgt->size)
    dbht_grow(tgt);
  
  if(width == 1) {
    tgt->data[tgt_idx] = src[src_start];
  } else if(width == 2) {
    unsigned int lchidx = dbht_l_child_index(tgt_idx);
    if(lchidx >= tgt->size)
      dbht_grow(tgt);
    tgt->data[tgt_idx] = src[src_start+1];
    tgt->data[lchidx] = src[src_start];
  } else if(width == 3) {
    unsigned int lchidx = dbht_l_child_index(tgt_idx);
    unsigned int rchidx = dbht_r_child_index(tgt_idx);
    if(rchidx >= tgt->size)
      dbht_grow(tgt);
    tgt->data[tgt_idx] = src[src_start+1];
    tgt->data[lchidx] = src[src_start];
    tgt->data[rchidx] = src[src_start+2];
  } else {
    unsigned int middle = (src_start + src_end) / 2;
    tgt->data[tgt_idx] = src[middle];
    dbht_entree(src, tgt, src_start, middle, dbht_l_child_index(tgt_idx));
    dbht_entree(src, tgt, middle+1, src_end, dbht_r_child_index(tgt_idx));
  }
  return;
}

/* Fully create a tree from an array of dbht_Cells. The returned value's
 * .data pointer will need free()ing.
 */
DBHT dbht_make_tree(dbht_Cell * arr, unsigned int siz) {
  DBHT new_tree = dbht_create(siz);
  quicksort_Cell_array(arr, siz);
  dbht_entree(arr, &new_tree, 0, siz, 0);
  return new_tree;
}

/* Return the cell with the given key, or one whose .value is NULL if
 * key is not present.
 */
dbht_Cell dbht_find(DBHT tree, unsigned int key) {
  unsigned int idx = 0;
  while(idx < tree.size) {
    if(tree.data[idx].value == NULL)
      return(tree.data[idx]);
    else if(tree.data[idx].key == key)
      return(tree.data[idx]);
    else if(tree.data[idx].key > key)
      idx = dbht_l_child_index(idx);
    else if(tree.data[idx].key < key)
      idx = dbht_r_child_index(idx);
    else
      die_on_error("dbht_find(_): loop fallthrough; should never be here");
  }
  return (dbht_Cell) { 0, NULL };
}

#ifdef DBHT_DEBUG

void dump_single_Cell(FILE * stream, const dbht_Cell cell) {
  if(cell.value == NULL)
    fprintf(stream, "x");
  else
    fprintf(stream, "%u", cell.key);
  return;
}

void dump_Cell_array(FILE * stream,
                     const dbht_Cell const * arr,
                     unsigned int siz) {
  if(siz < 1)
    fprintf(stream, "empty\n");
  else {
    fprintf(stream, "{ "); dump_single_Cell(stream, arr[0]);
    for(unsigned int n = 1; n < siz; ++n) {
      fprintf(stream, ", "); dump_single_Cell(stream, arr[n]);
    }
    fprintf(stream, " }\n");
  }
  return;
}

#endif

#ifdef DBHT_TEST

#include <time.h>
#define CELLS 64
#define R_MAX 24000

int main(int argc, char ** argv) {
  dbht_Cell * dat;
  unsigned int * dummy;
  unsigned int n, rnd;
  DBHT tree;
  dbht_Cell found;

  dat = malloc(sizeof(dbht_Cell) * CELLS);
  dummy = malloc(sizeof(unsigned int) * CELLS);

  srand(time(NULL));
  for(n = 0; n < CELLS; ++n) {
    dummy[n] = rand() % R_MAX;
    dat[n] = (dbht_Cell) { rand() % R_MAX, &dummy[n] };
  }

  dump_Cell_array(stdout, dat, CELLS);
  tree = dbht_make_tree(dat, CELLS);
  dump_Cell_array(stdout, dat, CELLS);
  dump_Cell_array(stdout, tree.data, tree.size);

  for(n = 3; n < 6; ++n) {
    found = dbht_find(tree, dat[n].key);
    printf("found %u: ", dat[n].key);
    dump_single_Cell(stdout, found);
    printf("\n");
  }
  for(n = CELLS - 3; n < CELLS; ++n) {
    found = dbht_find(tree, dat[n].key);
    printf("found %u: ", dat[n].key);
    dump_single_Cell(stdout, found);
    printf("\n");
  }

  found = dbht_find(tree, 215000);
  printf("found %u: ", 215000);
  dump_single_Cell(stdout, found);
  printf("\n");

  free(dat);
  free(dummy);

  return 0;
}

#endif /* DBHT_TEST */

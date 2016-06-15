/* dbht.c
 *
 * a "binary hash tree"
 *
 * REQUIRES
 * stdlib.h
 * stdio.h (if you want to test or debug)
 *
 * For use with urot13.c, to make code point lookups faster than O(n).
 *
 * The intended use case is to have a single large array of Unicode code
 * points, the number of combining characters it takes to make them, and
 * the sequence of combining code points that does it. For example:

 * unsigned int unidata[] = {
 *     531,    LATIN SMALL LETTER R WITH INVERTED BREVE        <- key
 *     2,      (requires 2 combining characters)               <- value target
 *     114,    LATIN SMALL LETTER R
 *     785,    COMBINING INVERTED BREVE
 *     199,    LATIN CAPITAL LETTER C WITH CEDILLA             <- key
 *     2,      (requires 2 combining characters)               <- value target
 *     67,     LATIN CAPITAL LETTER C
 *     807,    COMBINING CEDILLA
 *     554,    LATIN CAPITAL LETTER O WITH DIARESIS AND MACRON <- key
 *     3,      (requires 3 combining characters)               <- value target
 *     79,     LATIN CAPITAL LETTER O
 *     776,    COMBINING DIARESIS
 *     772,    COMBINING MACRON
 *     ... etc, }
 *
 * Each dbht_Node will contain an unsigned int key corresponding to the
 * precomposed Unicode code point, and value which is a pointer to the next
 * element in the array after the key's appearance.
 *
 * Upon program load, a loop will iterate through the array, building the
 * appropriate "hash tree" that points to places in the array.
 */

/* This is where we can define
 * DBHT_DEBUG
 * if we are so inclined.
 */

/* The unceremonious result of being out of memory.
 */
void dbht_die(const char * msg) {
  fprintf(stderr, "%s\n", msg);
  exit(EXIT_FAILURE);
}

typedef struct node {
  unsigned int key;
  const unsigned int * value;
  int l_height, r_height;
  struct node * l_tree;
  struct node * r_tree;
} dbht_Node;


int dbht_bias(const dbht_Node * tree) {
  return(tree->r_height - tree->l_height);
}

int dbht_height(const dbht_Node * tree) {
  if(tree == NULL)
    return(0);
  if(tree->l_height > tree->r_height) {
    return(tree->l_height);
  } else {
    return(tree->r_height);
  }
}

dbht_Node * dbht_new_node(unsigned int nkey,
                          const unsigned int * nvalue) {
  
  dbht_Node * nnp = malloc(sizeof(dbht_Node));
  if(nnp == NULL) {
    dbht_die("Unable to allocate memory for new dbht_node.");
  }

  nnp->key = nkey;
  nnp->value = nvalue;
  nnp->l_height = 1; nnp->r_height = 1;
  nnp->l_tree = NULL; nnp->r_tree = NULL;

  return(nnp);
}

/* Recursively free() all the memory used by all the subtrees of tree.
 * The root node will still have to be free()d.
 */
void dbht_destroy(dbht_Node * tree) {
  if(tree->l_tree != NULL) {
    dbht_destroy(tree->l_tree);
    free(tree->l_tree);
  }
  if(tree->r_tree != NULL) {
    dbht_destroy(tree->r_tree);
    free(tree->r_tree);
  }
  return;
}

/* Creates a new dbht_Node in the tree with the given nkey and nvalue.
 * Don't use this function; multiple calls will result in an unbalanced
 * tree. Use dbht_balanced_insert(), below.
 */
void dbht_insert(dbht_Node * tree,
                 unsigned int nkey,
                 const unsigned int * nvalue) {

  if(nkey == tree->key) {
    tree->value = nvalue;
  } else if(nkey < tree->key) {
    if(tree->l_tree == NULL) {
      tree->l_tree = dbht_new_node(nkey, nvalue);
    } else {
      dbht_insert(tree->l_tree, nkey, nvalue);
    }
    tree->l_height = dbht_height(tree->l_tree) + 1;
  } else {
    if(tree->r_tree == NULL) {
      tree->r_tree = dbht_new_node(nkey, nvalue);
    } else {
      dbht_insert(tree->r_tree, nkey, nvalue);
    }
    tree->r_height = dbht_height(tree->r_tree) + 1;
  }
  return;
}

void dbht_rotate_right(dbht_Node ** tree_ptr) {
  dbht_Node * old_root = *tree_ptr;
  dbht_Node * new_root = old_root->l_tree;
  dbht_Node * branch = new_root->r_tree;

  *tree_ptr = new_root;
  new_root->r_tree = old_root;
  old_root->l_tree = branch;
  old_root->l_height = dbht_height(branch) + 1;
  new_root->r_height = dbht_height(old_root) + 1;

  return;
}

void dbht_rotate_left(dbht_Node ** tree_ptr) {
  dbht_Node * old_root = *tree_ptr;
  dbht_Node * new_root = old_root->r_tree;
  dbht_Node * branch = new_root->l_tree;

  *tree_ptr = new_root;
  new_root->l_tree = old_root;
  old_root->r_tree = branch;
  old_root->r_height = dbht_height(branch) + 1;
  new_root->l_height = dbht_height(old_root) + 1;

  return;
}

/* This is how to properly insert stuff into the tree.
 */
void dbht_balanced_insert(dbht_Node ** tree_ptr,
                          unsigned int nkey,
                          const unsigned int * nvalue) {
  dbht_Node * tree = *tree_ptr;
  int new_bias;

  if(tree == NULL) {
    *tree_ptr = dbht_new_node(nkey, nvalue);
    return;
  } else if(nkey == tree->key) {
    tree->value = nvalue;
  } else if(nkey < tree->key) {
    if(tree->l_tree == NULL)
      tree->l_tree = dbht_new_node(nkey, nvalue);
    else
      dbht_balanced_insert(&(tree->l_tree), nkey, nvalue);
    tree->l_height = dbht_height(tree->l_tree) + 1;
  } else {
    if(tree->r_tree == NULL)
      tree->r_tree = dbht_new_node(nkey, nvalue);
    else
      dbht_balanced_insert(&(tree->r_tree), nkey, nvalue);
    tree->r_height = dbht_height(tree->r_tree) + 1;
  }

  new_bias = dbht_bias(tree);
  if(new_bias < -1) {
    if(dbht_bias(tree->l_tree) > 0)
      dbht_rotate_left(&tree->l_tree);
    dbht_rotate_right(tree_ptr);
  } else if(new_bias > 1) {
    if(dbht_bias(tree->r_tree) < 0)
      dbht_rotate_right(&tree->r_tree);
  dbht_rotate_left(tree_ptr);
  }

  return;
}

/* Return the dbht_Node whose key corresponds to k, or NULL if tree doesn't
 * contain such a node.
 */
dbht_Node * dbht_find(dbht_Node * tree,  unsigned int k) {
  if(tree == NULL)
    return NULL;
  else if(tree->key == k)
    return tree;
  else if(tree->key > k)
    return dbht_find(tree->l_tree, k);
  else
    return dbht_find(tree->r_tree, k);
}

#ifdef DBHT_DEBUG

/* These are functions that can be used by a debugging program. */

void print_indent(unsigned int n) {
  unsigned int i;
  for(i = 0; i < n; ++i) printf(" ");
  return;
}

void dbht_print_indent(dbht_Node * tree, unsigned int lvl) {
  print_indent(lvl);
  if(tree == NULL) {
    printf("{ }\n");
  } else if((tree->l_tree == NULL) && (tree->r_tree == NULL)) {
    printf("{ %u: %p; %d %d {} {} }\n", tree->key, tree->value,
           tree->l_height, tree->r_height);
  } else {
    printf("{ %u: %p; %d %d\n", tree->key, tree->value,
           tree->l_height, tree->r_height);
    dbht_print_indent(tree->l_tree, lvl+2);
    dbht_print_indent(tree->r_tree, lvl+2);
    print_indent(lvl);
    printf("}\n");
  }
  return;
}

void dbht_print(dbht_Node * tree) {
  dbht_print_indent(tree, 0);
}

void dbht_print_one(dbht_Node * tree) {
  if(tree == NULL) {
    printf("{ NULL }");
  } else {
    printf("{ %u: %p; %d %d }", tree->key, tree->value,
           tree->l_height, tree->r_height);
  }
  return;
}

void dbht_walk(dbht_Node * tree,
               unsigned int * tgt,
               size_t * count) {
  if(tree->l_tree != NULL) {
    dbht_walk(tree->l_tree, tgt, count);
  }
  tgt[*count] = tree->key;
  *count = (*count) + 1;
  if(tree->r_tree != NULL) {
    dbht_walk(tree->r_tree, tgt, count);
  }
}

unsigned int * dbht_list_keys(dbht_Node * tree,
                              unsigned int * len) {
  unsigned int temp_len, n, height;
  size_t count;
  unsigned int * tgt;
  unsigned int * ret_val;
  temp_len = 1;
  height = dbht_height(tree);
  for(n=0; n < height; ++n)
    temp_len = temp_len * 2;
  tgt = malloc(sizeof(unsigned int) * temp_len);
  count = 0;
  dbht_walk(tree, tgt, &count);
  ret_val = malloc(sizeof(unsigned int) * count);
  for(n = 0; n < count; ++n)
    ret_val[n] = tgt[n];
  free(tgt);
  *len = count;
  return ret_val;
}

#endif

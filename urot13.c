/* urot13.c
 *
 * a Unicode-aware rot13 implementation
 */

#include <stdlib.h>
#include <stdio.h>
#include "unicodec.c"
#include "dbht.c"
#include "unicode_data.c"

UNICHAR const RANGES[] = {
  0x0041,    /* A */
  0x0061,    /* a */
  0x249c,    /* parenthesized latin small letter a */
  0x24b6,    /* circled latin capital letter a */
  0x24d0,    /* circled latin small letter a */
  0xff21,    /* fullwidth latin capital letter a */
  0xff41,    /* fullwidth latin small letter a */
  0x1f110,   /* parenthesized latin capital letter a */
  0x1f310,   /* squared latin capital letter a */
  0x1f150,   /* negative circled latin capital letter a */
  0x1f170,   /* negative squared latin capital letter a */
  0x1f1e6    /* regional indicator symbol letter a */
};

size_t const RANGES_LEN = 12;

dbht_Node * unitree = NULL;

char const * const OM_ERROR = "Unable to allocate memory, which is dumb.";

void die_unnatural_death(char const * const msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

UNICHAR determine_range(UNICHAR cp) {
  size_t n;
  for(n = 0; n < RANGES_LEN; ++n) {
    if(cp < RANGES[n])
      return 0;
    else if(cp < RANGES[n] + 26)
      return RANGES[n];
  }
  return 0;
}

UNICHAR range_rotate(UNICHAR cp) {
  UNICHAR low = determine_range(cp);
  if(low == 0)
    return cp;
  else
    return low + ((cp - low + 13) % 26);
}

size_t unicode_rot13(UNICHAR cp, UNICHAR * buff) {
  dbht_Node * node = dbht_find(unitree, cp);
  if(node == NULL) {
    buff[0] = range_rotate(cp);
    return 1;
  } else {
    unsigned int n_chars = (node->value)[0];
    unsigned int * val_vec = &node->value[1];
    buff[0] = range_rotate((UNICHAR) val_vec[0]);
    for(unsigned int n = 1; n < n_chars; ++n)
      buff[n] = (UNICHAR) val_vec[n];
    return (size_t) n_chars;
  }
}

char * munch_line(char * line, size_t in_len, size_t * out_len) {
  size_t ub_len = 0;
  size_t out_n = 0;
  UNICHAR * unibuff = NULL;
  UNICHAR * n_unibuff = NULL;
  char * outbuff;

  unibuff = utf8_decode_buffer(line, in_len, &ub_len);
  if(unibuff == NULL)
    die_unnatural_death(OM_ERROR);
  n_unibuff = malloc(sizeof(UNICHAR) * ub_len * 4);
  if(unibuff == NULL)
    die_unnatural_death(OM_ERROR);

  {
    size_t in_n, n_points, n;
    UNICHAR tmpbuff[4] = {0, 0, 0, 0};
    for(in_n = 0; in_n < ub_len; ++in_n) {
      n_points = unicode_rot13(unibuff[in_n], tmpbuff);
      for(n = 0; n < n_points; ++n) {
        n_unibuff[out_n] = tmpbuff[n];
        ++out_n;
      }
    }
  }
  free(unibuff);

  outbuff = utf8_encode_buffer(n_unibuff, out_n, out_len);
  if(outbuff == NULL)
    die_unnatural_death(OM_ERROR);
  free(n_unibuff);
  return outbuff;
}

int main(int argc, char ** argv) {
  char * in_line = NULL;
  char * out_line = NULL;
  int in_len = 0;
  size_t out_len = 0;
  size_t n = 0;

  {
    char cont = 1;
    unsigned int n = 0;
    unsigned int ch;
    unsigned int * ptr;

    while(cont) {
      if(unidata[n] == 0) {
        cont = 0;
      } else {
        ch = unidata[n];
        ++n;
        ptr = &unidata[n];
        n = n + unidata[n] + 1;
      }
      dbht_balanced_insert(&unitree, ch, ptr);
    }
  }

  while((in_len = getline(&in_line, &n, stdin)) != -1) {
    out_line = munch_line(in_line, (size_t) in_len, &out_len);
    printf("%s", out_line);
    free(out_line);
  }

  free(in_line);
  dbht_destroy(unitree);
  free(unitree);
  
  return 0;
}

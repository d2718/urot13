/* urot13.c
 *
 * a Unicode-aware rot13 implementation
 *
 * last update: 2016-06-25
 *
 * Reads UTF-8 encoded data from the standard input. If any code points can be
 * represented as a rot13-able base character followed by combining diacritics,
 * break it thus down, rot13 the base character, and follow it with the
 * diacritics. Writes UTF-8 to the standard output.
 *
 * Also, of course, rot13s appropriate ASCII characters, as well as characters
 * from the Enclosed Alphanumerics, Enclosed Alphanumerics Supplement, and
 * CJK Fullwidth Forms blocks.
 *
 * Internally, this program stores code points as unsigned ints. If your
 * architecture can't fit all 21 bits into an unsigned int, you may need to
 * consider some changes. The first change I would consider would be not
 * using your toaster to perform trivial text obfuscations. If, at that point,
 * you haven't been dissuaded, you're going to need to turn those unsigned
 * ints (including the UNICHAR typedef in unicodec.c) into something else.
 * Best of luck with your ridiculousness.
 */

#include <stdlib.h>
#include <stdio.h>
#include "unicodec.c"
#include "unicode_data.c"

/* The lowest code point of each range of characters that could and
 * should get rot13'd.
 */
const UNICHAR const RANGES[] = {
  0x0041,    /* A */
  0x0061,    /* a */
  0x249c,    /* parenthesized latin small letter a */
  0x24b6,    /* circled latin capital letter a */
  0x24d0,    /* circled latin small letter a */
  0xff21,    /* fullwidth latin capital letter a */
  0xff41,    /* fullwidth latin small letter a */
  0x1f110,   /* parenthesized latin capital letter a */
  0x1f130,   /* squared latin capital letter a */
  0x1f150,   /* negative circled latin capital letter a */
  0x1f170,   /* negative squared latin capital letter a */
  0x1f1e6    /* regional indicator symbol letter a */
};

/* The length of RANGES[] */
const size_t RANGES_LEN = 12;

/* Array of code points that get decomposed. */
UNICHAR * decomp_points;

/* Array of pointers to data about how each code point gets decomposed. */
const unsigned int ** decomp_values;

/* Data potato doo-wop doo-wop */
const char * const OM_ERROR = "Unable to allocate memory, which is dumb.";

/* It's easier to just give up in the case of errors. Sure, I'm lazy, but I
 * really think it'd violate the Principle of Least Surprise if it did
 * anything else.
 */
void die_unnatural_death(const char * const msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}

/* If the code point key is decomposable into something rot13able, return
 * a pointer to the appropriate decomposition info, otherwise, return NULL.
 */
const unsigned int * treelike_search(UNICHAR key, size_t lo, size_t hi) {
  size_t length = hi - lo;
  if(length < 2) {
    if(decomp_points[lo] == key)
      return decomp_values[lo];
    else
      return NULL;
  } else {
    size_t mid = lo + (length / 2);
    if(key < decomp_points[mid])
      return treelike_search(key, lo, mid);
    else if(key > decomp_points[mid])
      return treelike_search(key, mid + 1, hi);
    else
      return decomp_values[mid];
  }
}

/* If cp falls in a range that should get rot13'd, return the lowest code
 * point in that range (because it's what the rot13 calculation needs);
 * otherwise, return 0.
 */
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

/* If cp falls in a range that should be rotated, do so; otherwise, just
 * pass it through.
 */
UNICHAR range_rotate(UNICHAR cp) {
  UNICHAR low = determine_range(cp);
  if(low == 0)
    return cp;
  else
    return low + ((cp - low + 13) % 26);
}

/* Write the series of code points that combine to make the rot13'd version
 * of cp to buff; return the number of characters this is. Most of the time,
 * it'll probably just be 1.
 */
size_t unicode_rot13(UNICHAR cp, UNICHAR * buff) {
  const unsigned int * chptr = treelike_search(cp, 0, unidata_size);
  if(chptr == NULL) {
    buff[0] = range_rotate(cp);
    return 1;
  } else {
    const unsigned int n_chars = chptr[0];
    const unsigned int * val_vec = &chptr[1];
    buff[0] = range_rotate((UNICHAR) val_vec[0]);
    for(unsigned int n = 1; n < n_chars; ++n)
      buff[n] = (UNICHAR) val_vec[n];
    return (size_t) n_chars;
  }
}

/* Read a line of UTF-8 data, rot13 it appropriately, reëncode the output,
 * null-terminate it, and return a pointer to the new data. Write the number
 * of bytes in the returned buffer to *out_len.
 * REMINDER!
 * The returned pointer will need to be free()d!
 */
char * munch_line(const char * line, size_t in_len, size_t * out_len) {
  size_t ub_len = 0;
  size_t out_n = 0;
  UNICHAR * unibuff = NULL;
  UNICHAR * n_unibuff = NULL;
  char * outbuff;

  unibuff = utf8_decode_buffer(line, in_len, &ub_len);
  if(unibuff == NULL)
    die_unnatural_death(OM_ERROR);
  n_unibuff = malloc(sizeof(UNICHAR) * ub_len * 4 + 1);
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
    n_unibuff[out_n] = 0;
    ++out_n;
  }
  free(unibuff);

  outbuff = utf8_encode_buffer(n_unibuff, out_n, out_len);
  if(outbuff == NULL)
    die_unnatural_death(OM_ERROR);
  free(n_unibuff);
  return outbuff;
}

/* Read UTF-8 encoded data one line at a time from the standard input and
 * write the rot13'd version reëncoded back to the standard output.
 */
int main(int argc, char ** argv) {
  char * in_line = NULL;
  char * out_line = NULL;
  int in_len = 0;
  size_t out_len = 0;
  size_t n = 0;

  decomp_points = malloc(sizeof(UNICHAR) * unidata_size);
  decomp_values = malloc(sizeof(unsigned int *) * unidata_size);

  {
    char cont = 1;
    size_t m = 0;

    while(cont) {
      if(unidata[n] == 0) {
        cont = 0;
      } else {
        decomp_points[m] = unidata[n];
        ++n;
        decomp_values[m] = &unidata[n];
        n = n + unidata[n] + 1;
        ++m;
      }
    }
  }

  while((in_len = getline(&in_line, &n, stdin)) != -1) {
    out_line = munch_line(in_line, (size_t) in_len, &out_len);
    printf("%s", out_line);
    free(out_line);
  }

  free(in_line);
  free(decomp_points);
  free(decomp_values);
  
  return 0;
}

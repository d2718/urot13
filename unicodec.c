
/* unicodec.c
 *
 * A simple(?) UTF-8 encoder and decoder.
 *
 * last update: 2016-06-13
 *
 * REQUIRES
 * stdlib.h
 * stdio.h (if you want test or debug)
 *
 * This implementation stores Unicode code points as unsigned ints. If an
 * unsigned int is less than four bytes on your hardware, you may want to
 * change the typedef of UNICHAR. You may also want to ask yourself why
 * your toaster needs to be Unicode-aware.
 *
 * Also, all the C input and output functions deal with streams of char,
 * but internally these are all cast to unsigned char, just because it
 * was easier for me to think about it that way. I'm pretty sure under the
 * hood it doesn't matter.
 *
 * CAVEAT PROGRAMMOR!
 *
 * The two real functions here,
 *
 * UNICHAR * utf8_decode_buffer(char * in_buffer,
 *                              size_t in_len,
 *                              size_t * out_len)
 *
 * and
 *
 * char * utf8_encode_buffer(UNICHAR * buff,
 *                           size_t in_len,
 *                           size_t * out_len)
 *
 * both malloc() their own buffers of exactly the right size and return
 * pointers to them. These pointers will need to be free()d! (The buffer
 * size is written to the address of out_len).
 */

/* If we want to debug or test, this is where we define
 * UNICODEC_DEBUG
 * UNICODEC_TEST
 */

typedef unsigned int UNICHAR;

const unsigned char head2 = 192;    /* 110xxxxx */
const unsigned char head3 = 224;    /* 1110xxxx */
const unsigned char head4 = 240;    /* 11110xxx */
const unsigned char headcont = 128; /* 10xxxxxx */

const unsigned char mask_high1 = 128;
const unsigned char mask_high2 = 192;
const unsigned char mask_high3 = 224;
const unsigned char mask_high4 = 240;
const unsigned char mask_high5 = 248;

const unsigned char mask_low3 = 7;
const unsigned char mask_low4 = 15;
const unsigned char mask_low5 = 31;
const unsigned char mask_low6 = 63; 

const UNICHAR ureplacement = 0xfffd;

UNICHAR unichar_assemble(char * b) {
  unsigned int x[4];
  unsigned char n;

  for(n = 0; n < 4; ++n) x[n] = (unsigned int) b[n];
  return(x[0] | (x[1]<<6) | (x[2]<<12) | (x[3]<<18));
}

UNICHAR next_codepoint(char * stream, size_t *bytes_left) {
  char first = stream[0];
  char bytes[4] = {0, 0, 0, 0};
  char first_mask, n_bytes, n, m, tmp;
  size_t b = *bytes_left;

  if((first & mask_high1) == 0) {
    *bytes_left = b - 1;
    return (UNICHAR) stream[0];
  } else if((first & mask_high3) == head2) {
    n_bytes = 2; first_mask = mask_low5;
  } else if((first & mask_high4) == head3) {
    n_bytes = 3; first_mask = mask_low4;
  } else if((first & mask_high5) == head4) {
    n_bytes = 4; first_mask = mask_low3;
  } else {
    *bytes_left = b - 1;
    return(ureplacement);
  }

  if (n_bytes > b) {
    *bytes_left = b - 1;
    return(ureplacement);
  }

  m = 0;
  for(n = n_bytes-1; n > 0; --n) {
    tmp = stream[n];
    if((tmp & mask_high2) == headcont) {
      bytes[m] = (tmp & mask_low6);
      ++m;
    } else {
      *bytes_left = b - n;
      return(ureplacement);
    }
  }
  bytes[m] = (first & first_mask);
  *bytes_left = b - n_bytes;
  return(unichar_assemble(bytes));
}

/* Will decode an array of utf-8-encoded bytes, buff, into an array of Unicode
 * code points, and return a pointer to the array. Requires the number of bytes
 * in buff, passed as in_len. Writes the length of the buffer returned into
 * the address of out_len.
 */
UNICHAR * utf8_decode_buffer(char * buff,
                             size_t in_len,
                             size_t *out_len) {
  size_t bytes_left = in_len;
  UNICHAR target;
  UNICHAR * temp;
  UNICHAR * out_buffer;
  size_t written = 0;

#ifdef UNICODEC_DEBUG
  fprintf(stderr, "in utf8_decode_buffer(%p, %lu, %p):\n",
          buff, in_len, out_len);
#endif

  temp = malloc(sizeof(UNICHAR) * in_len);
  while(bytes_left > 0) {

#ifdef UNICODEC_DEBUG
    fprintf(stderr, "  bytes_left: %lu\n", bytes_left);
#endif
    
    target = next_codepoint(&(buff[in_len-bytes_left]),
                            &bytes_left);
#ifdef UNICODEC_DEBUG
    fprintf(stderr, "  decoded: %u; bytes_left: %lu\n", target, bytes_left);
#endif
    temp[written] = target;
    ++written;
  }

  out_buffer = malloc(sizeof(UNICHAR) * written);
  for(size_t n = 0; n < written; ++n)
    out_buffer[n] = temp[n];
  free(temp);
  *out_len = written;

  return(out_buffer);
}


/* Encode Unicode code point ch in char array target, which must be at
 * least 4 bytes long. Returns the number of bytes used to encode ch.
 */
unsigned char utf8_encode_point(UNICHAR ch, unsigned char * target) {

  if(ch < 0x0080) {
    target[0] = (unsigned char) ch;
    return(1);
  } else if(ch < 0x0800) {
    target[0] = (unsigned char) ((ch>>6) | head2);
    target[1] = headcont | ((unsigned char) (ch & mask_low6));
    return(2);
  } else if(ch < 0x10000) {
    target[0] = (unsigned char) ((ch>>12) | head3);
    target[1] = headcont | ((unsigned char) ((ch>>6) & mask_low6));
    target[2] = headcont | ((unsigned char) (ch & mask_low6));
    return(3);
  } else if(ch < 0x200000)  {
    target[0] = (unsigned char) ((ch>>18) | head4);
    target[1] = headcont | ((unsigned char) ((ch>>12) & mask_low6));
    target[2] = headcont | ((unsigned char) ((ch>>6) & mask_low6));
    target[3] = headcont | ((unsigned char) (ch & mask_low6));
  } else {
    return(utf8_encode_point(ureplacement, target));
  }
}

/* Encode an array of Unicode code points, buff, into a byte array, a
 * pointer to which is returned. Requires the length of buff, passed as
 * in_len; the length of the returned array is written to *out_len.
 */
char * utf8_encode_buffer(UNICHAR * buff,
                          size_t in_len,
                          size_t * out_len) {
  unsigned char bytes[] = {0, 0, 0, 0};
  size_t in_n, out_n, n;
  unsigned char n_bytes;
  unsigned char * temp;
  unsigned char * out_buff;

  temp = malloc(sizeof(unsigned char) * in_len * 4);

  out_n = 0;
  for(in_n = 0; in_n < in_len; ++in_n) {
    n_bytes = utf8_encode_point(buff[in_n], bytes);
#ifdef DDEBUG
    fprintf(stderr, "code point %u requires %hhu byte(s)\n", buff[in_n], n_bytes);
#endif
    for(n = 0; n < n_bytes; ++n) {

#ifdef DDEBUG
      fprintf(stderr, "writing %hhu to temp[%u]\n", bytes[n], out_n);
#endif
      
      temp[out_n] = bytes[n];
      ++out_n;
    }
  }

  out_buff = malloc(sizeof(unsigned char) * (out_n + 1));
  for(n = 0; n < out_n; ++n)
    out_buff[n] = temp[n];
  out_buff[out_n] = 0;
  free(temp);
  *out_len = out_n;
  return out_buff;
}

#ifdef UNICODEC_DEBUG

/* Write a string of the bit values in c to dest, which should be at least
 * eight bytes long.
 */
void char2bin(char c, char * dest)
{
  char base = 1;
  int n = 0;
  for(n=7; n >= 0; --n) {
    if(base & c) {
      dest[n] = '1';
    } else {
      dest[n] = '0';
    }
    base = base * 2;
  }
  dest[8] = 0;
}

#endif

#ifdef UNICODEC_TEST

/* This test main() function reads lines from the standard input, decodes
 * from UTF-8, adds one to each code point 65 ('A') and above, then writes
 * the re-encoded result to the standard output.
 */ 
int main(int argc, char **argv) {
    char * line = NULL;
    char * out;
    size_t n = 0;
    size_t i = 0;
    size_t amt_read = 0;
    UNICHAR * upoints;

    while((amt_read = getline(&line, &n, stdin)) != -1) {
      upoints = utf8_decode_buffer(line, amt_read, &n);
      for(i = 0; i < n; ++i)
        printf("%u ", upoints[i]);
      printf("\n\n");
      for(i = 0; i < n; ++i) {
        if(upoints[i] >= 65)
          upoints[i] = upoints[i] + 1;
      }
      for(i = 0; i < n; ++i)
        printf("%u ", upoints[i]);
      printf("\n\n");

      out = utf8_encode_buffer(upoints, n, &i);

      printf("%s\n", out);
      
      free(out);
      free(upoints);
    }

    free(line);
    exit(0);
}

#endif /* UNICODEC TEST */

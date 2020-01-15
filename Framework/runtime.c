#include <stdio.h>
#include <stdlib.h>
 

extern long _scheme_entry(int* stack, int* heap); 
	
void
	print(long x) {
	  printf("%ld\n", x);
	}
	
	int
	main(int argc, char *argv[]) {
	  if (argc != 1) {
	    fprintf(stderr, "usage: %s\n", argv[0]);
	    exit(1);
	  }
	  int* rdi = (int*)malloc(1024*1024);
	  int * rsi = (int*)malloc (1024*1024);
	  print(_scheme_entry(rdi, rsi));
	  return 0;
	}
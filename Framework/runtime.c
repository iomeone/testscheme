#include <stdio.h>
#include <stdlib.h>
 

extern long _scheme_entry(); 
	
void
	print(long x) {
	  printf("%ld", x);
	}
	
	int
	main(int argc, char *argv[]) {
	  if (argc != 1) {
	    fprintf(stderr, "usage: %s\n", argv[0]);
	    exit(1);
	  }
	  long x = _scheme_entry();
	  print(x);
	  fflush(stdin);
	  return 0;
	}
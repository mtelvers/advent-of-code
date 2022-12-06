#!/usr/bin/gawk -f

BEGIN {
  highest = 1;
  e = 1;
}

/^$/ {
  if (elf[e] > elf[highest]) {
    highest = e;
  }
  e = e + 1;
  next
}

/[[:digit:]]*/ {
  elf[e] += $1
}

END {
  print elf[highest]
}

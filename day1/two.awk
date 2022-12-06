#!/usr/bin/gawk -f

BEGIN {
  e = 1;
}

/^$/ {
  e = e + 1;
  next
}

/[[:digit:]]*/ {
  elf[e] += $1
}

END {
  n = asort(elf);
  for (i = n - 2; i <= n; i++)
    total += elf[i]
  print "Top three total: " total
}

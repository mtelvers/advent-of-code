#!/usr/bin/gawk -f

BEGIN {
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
}

function uniq(str, str2, str3) {
  for (i = 1; i <= length(str); i++)
    for (j = 1; j <= length(str2); j++)
      for (k = 1; k <= length(str3); k++)
        if ((substr(str, i, 1) == substr(str2, j, 1)) && (substr(str, i, 1) == substr(str3, k, 1)))
          return i
  return -1
}

(NR - 1) % 3 == 0 {
  bag1 = $0
}

(NR - 1) % 3 == 1 {
  bag2 = $0
}

(NR - 1) % 3 == 2 {
  bag3 = $0;
  item = substr(bag1, uniq(bag1, bag2, bag3), 1);
  priority += index(alphabet, item);
}

END {
  print "Part two: " priority
}

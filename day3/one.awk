#!/usr/bin/gawk -f

BEGIN {
  alphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
}

function uniq(str, str2) {
  for (i = 1; i <= length(str); i++)
    for (j = 1; j <= length(str2); j++)
      if (substr(str, i, 1) == substr(str2, j, 1))
        return i
  return -1
}

{
  half = length($0) / 2;
  sack1 = substr($0, 1, half);
  sack2 = substr($0, half + 1);
  if (uniq(sack1, sack2) >= 0) {
    item = substr(sack1, i, 1);
    priority += index(alphabet, item);
  }
}

END {
  print "Part one: " priority
}

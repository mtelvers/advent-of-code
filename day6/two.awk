#!/usr/bin/gawk -f

function uniq(str) {
  for (i = 1; i <= length(str); i++)
    for (j = i + 1; j <= length(str); j++)
      if (substr(str, i, 1) == substr(str, j, 1))
        return 0
  return 1
}

{
  for (x = 1; x <= length($0) - 14; x++) {
    if (uniq(substr ($0, x, 14))) {
      print x + 13;
      next;
    }
  }
}

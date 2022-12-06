#!/usr/bin/gawk -f

BEGIN {
  FS="[-,]"
}

{
  duplicates = 0;
  for (i = $1; i <= $2; i++)
    if (($3 <= i) && (i <= $4))
      duplicates++;
  if ((duplicates == ($2 - $1 + 1)) || (duplicates == ($4 - $3 + 1)))
    spare++;
  if (duplicates > 0)
    overlap++;
}

END {
  print "Spare elves: " spare
  print "Some overlap elves: " overlap
}

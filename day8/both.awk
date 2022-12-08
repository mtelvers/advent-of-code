#!/usr/bin/gawk -f

{
  split($0, chars, "");
  for (col in chars)
    forest[col][NR] = chars[col]
}

function find_visible(ix, iy, dx, dy, h) {
  ix += dx;
  iy += dy;
  if ((ix < 1) || (ix > NR)) return 1;
  if ((iy < 1) || (iy > NR)) return 1;
  if (h > forest[ix][iy])
    return find_visible(ix, iy, dx, dy, h)
  else
    return 0;
}

function find_scenic(ix, iy, dx, dy, h, r) {
  ix += dx;
  iy += dy;
  if ((ix < 1) || (ix > NR)) return r;
  if ((iy < 1) || (iy > NR)) return r;
  if (h > forest[ix][iy])
    return find_scenic(ix, iy, dx, dy, h, r + 1)
  else
    return r + 1;
}

END {
  for (x in forest) {
    for (y in forest[x]) {
      visible = find_visible(x, y, 0, -1, forest[x][y]) + \
                find_visible(x, y, 0, 1, forest[x][y]) + \
                find_visible(x, y, -1, 0, forest[x][y]) + \
                find_visible(x, y, 1, 0, forest[x][y])
      if (visible > 0)
        visible_trees++;
    }
  }
  print "Visible trees: " visible_trees

  for (x in forest) {
    for (y in forest[x]) {
      scenic = find_scenic(x, y, 0, -1, forest[x][y], 0) * \
               find_scenic(x, y, 0, 1, forest[x][y], 0) * \
               find_scenic(x, y, -1, 0, forest[x][y], 0) * \
               find_scenic(x, y, 1, 0, forest[x][y], 0)
      if (scenic > most_scenic)
        most_scenic = scenic
    }
  }
  print "Most scenic: " most_scenic
}

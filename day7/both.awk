#!/usr/bin/gawk -f

function up() {
  s = sum[path];
  n = split(path, elements, "/");
  path = "/"
  for (i = 2; i < n - 1; i++)
    path = path elements[i] "/";
  sum[path] += s;
}

$1 == "$" && $2 == "cd" {
  switch ($3) {
    case "/":
      path = $3;
      break;
    case "..":
      up();
      break;
    default:
      path = path $3 "/"
      break;
  }
}

$1 == "$" && $2 == "ls" {
  next;
}

/^dir/ {
  next;
}

{
  sum[path] += $1
}

END {
  while (n = split(path, elements, "/") > 2) {
    print path ": " n;
    up();
  }
  for (i in sum) {
    print i " " sum[i]
  }

  total = 0;
  for (i in sum) {
    if (sum[i] <= 100000)
      total += sum[i];
  }
  print "part one total: " total

  available = 70000000 - sum["/"];
  print "available space: " available

  needed = 30000000 - available;

  for (i in sum) {
    if (sum[i] > needed) {
      if (delete_me) {
        if (sum[i] < sum[delete_me])
          delete_me = i;
      } else {
        delete_me = i;
      }
      print "big enough " i " " sum[i] " (smallest is " sum[delete_me] ")"
    }
  }

  print "part two - delete: " delete_me " of size " sum[delete_me]
}

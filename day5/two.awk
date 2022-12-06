#!/usr/bin/gawk -f

/\[/ {
  for (i = 0; i < 10; i++) {
    ch = substr($0, 2 + i * 4, 1);
    if (ch != " ")
      stacks[i + 1] = stacks[i + 1] substr($0, 2 + i * 4, 1)
  }
}

/move/ {
  tmp = substr(stacks[$4], 1, $2);
  stacks[$4] = substr(stacks[$4], $2 + 1)
  stacks[$6] = tmp stacks[$6]
}

END {
  for (i = 1; i < 10; i++)
    printf substr(stacks[i], 1, 1);
  printf "\n"
}

{
  int x;
  x = 0;
  while (rand(0,1) == 0) {
    x = x + 2;
  }
  if (x > 0) {
    print(x);
    assert(x > 1);
  }
}

/*
 * Polymorphism:
 *  - derived class contains an overriding method
 *  - the overriding method doesn't contain `virtual` keyword
 */
#include <cassert>

class Bird
{
public:
  virtual int do_something(void)
  {
    return 21;
  }
  virtual int do_something(int)
  {
    return 22;
  }
};

class Penguin : public Bird
{
public:
  int do_something(void)
  {
    return 42;
  }
  int do_something(int)
  {
    return 43;
  }
};

int main()
{
  Bird *b = new Bird();
  Bird *p = new Penguin();
  assert(b->do_something() == 21);
  assert(b->do_something(1) == 22);
  assert(p->do_something() == 21); // FAIL, should be 42
  delete b;
  delete p;
  return 0;
}

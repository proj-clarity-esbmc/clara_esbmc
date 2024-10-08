/*
 * Polymorphism:
 *  - derived class contains an overriding method
 *  - the overriding method has `virtual` keyword
 *  - derived class has non_trivial constructor
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
  Penguin(){};
  virtual int do_something(void)
  {
    return 42;
  }
  virtual int do_something(int)
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
  assert(p->do_something() == 42);
  assert(p->do_something(1) == 43);
  delete b;
  delete p;
  return 0;
}

/*
 * multi-level inheritance, single column
 */
#include <cassert>

class File
{
public:
  virtual int f(void)
  {
    return 1;
  }
  virtual int f(int)
  {
    return 2;
  }
};

class InputFile : public File
{
public:
  virtual int f(void)
  {
    return 10;
  }
  virtual int f(int)
  {
    return 20;
  }
};

class IOFile : public InputFile
{
public:
  virtual int f(void)
  {
    return 100;
  }
  virtual int f(int)
  {
    return 200;
  }
};

int main()
{
  InputFile *iofile2 = new IOFile();
  assert(iofile2->File::f() == 1);
  assert(iofile2->File::f(1) == 2);
  assert(iofile2->InputFile::f() == 10);
  assert(iofile2->InputFile::f(1) == 20);
  assert(
    iofile2->f() !=
    1); // make sure it's not calling the top-level base function
  assert(
    iofile2->f(1) !=
    2); // make sure it's not calling the top-level base function
  assert(
    iofile2->f() !=
    10); // make sure it's not calling the second-level base function
  assert(
    iofile2->f(1) !=
    20); // make sure it's not calling the second-level base function
  assert(iofile2->f() == 100);
  assert(iofile2->f(1) == 200);
  delete iofile2;

  return 0;
}

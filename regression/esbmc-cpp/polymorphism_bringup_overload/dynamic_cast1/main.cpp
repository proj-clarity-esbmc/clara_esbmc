/*
 * Single inheritance, dynamic cast, late binding
 */
#include <cassert>

class CPolygon
{
protected:
  int width, height;

public:
  CPolygon()
  {
  }
  void set_values(int a, int b)
  {
    width = a;
    height = b;
  }
  virtual int area(void) = 0;
  virtual int area(int) = 0;
};

class CRectangle : public CPolygon
{
public:
  CRectangle(int w, int h)
  {
    width = w;
    height = h;
  }
  int area(void)
  {
    return (width * height);
  }
  int area(int a)
  {
    return (width * height * a);
  }
};

class CTriangle : public CPolygon
{
public:
  CTriangle(int w, int h)
  {
    width = w;
    height = h;
  }
  int area(void)
  {
    return ((width * height) / 2);
  }
  int area(int a)
  {
    return ((width * height * a) / 2);
  }
};

int main()
{
  CPolygon *polygons[2];

  polygons[0] = new CTriangle(20, 25);  //CRectangle(20,30);
  polygons[1] = new CRectangle(20, 30); //new CTriangle(20,25);

  for (int i = 0; i < 2; i++)
  {
    CTriangle *trin = dynamic_cast<CTriangle *>(polygons[i]);
    if (trin != 0)
    {
      assert(trin->area() == 250);  // PASS
      assert(trin->area(2) == 500); // PASS

      trin->set_values(10, 10);     // access base method from the casted ptr
      assert(trin->area() == 50);   // PASS
      assert(trin->area(2) == 100); // PASS
    }
  }

  for (int i = 0; i < 2; i++)
    delete polygons[i];

  return 0;
}

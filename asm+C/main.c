extern "C" void print (int a); // Stop name mangling games

int main() {
  int a = 42;
  print (a);
  
  return 0;
}

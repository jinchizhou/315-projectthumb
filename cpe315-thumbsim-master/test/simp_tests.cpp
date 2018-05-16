
class SimpleTest : public CppUnit::TestCase {
   public:
      SimpleTest( std::string name ) : CppUnit::TestCase( name ) {}

      void runTest() {
         CPPUNIT_ASSERT( Complex (10, 1) == Complex (10, 1) );
         CPPUNIT_ASSERT( !(Complex (1, 1) == Complex (2, 2)) );
      }
};



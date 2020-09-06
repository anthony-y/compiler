// This code was generated by a compiler from scratch/1.lang
// http://github.com/anthony-y/compiler
typedef signed   char s8;
typedef unsigned char u8;
typedef signed   short s16;
typedef unsigned short u16;
typedef signed   int s32;
typedef unsigned int u32;
typedef signed   long int s64;
typedef unsigned long int u64;
typedef float  f32;
typedef double f64;
typedef struct string {const u8 *data; u64 length;} string;
typedef enum bool {false, true} bool;
#define cast 
#define NULL (void *)0

void __global_initializers();
static inline string __make_string(const u8 *data, u64 length) {
    return (string){.data=data, .length=length};
}

void compiler_main();
typedef int T;
struct Field;
typedef struct Field {
    string* name;
    int not_ptr;
}Field;

void __compiler_main() {
    __global_initializers();
    string args[1024];
    args[10] = __make_string("", 0);
    int i[1024];
    i[0] = 10;
    int sum = -i[0];
}

void __global_initializers() {
}
int main(int __argcount, char *__args[]) {
    __compiler_main();
}

// This code was generated by a compiler from demos/hello.lang
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
typedef struct string {u8 *data; u64 length;} string;
typedef enum bool {false, true} bool;
#define cast 
#define NULL (void *)0

void __global_initializers();
static inline string __make_string(u8 *data, u64 length) {
    return (string){.data=data, .length=length};
}

void printf(u8* fmt, ...);
void compiler_main();


void __compiler_main() {
__global_initializers();
printf(__make_string("Hello, world\n", 14).data);
}

void __global_initializers() {
}
int main(int __argcount, char *__args[]) {
__compiler_main();
}

// This code was generated by a compiler from scratch/scratch.lang
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

int i;
int j;
void compiler_main();
void use_i(int n);
string get_message();

void __compiler_main() {
    __global_initializers();
    string message = get_message();
    use_i(i);
    j = 10;
    string b = __make_string("testing symbol resolution", 25);
    string a = b;
}
void use_i(int n) {
}
string get_message() {
    get_message();
    return __make_string("Hi", 2);
}

void __global_initializers() {
    i = 10;
    j = i;
}
int main(int __argcount, char *__args[]) {
    __compiler_main();
}

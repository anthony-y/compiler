// This code was generated by a compiler, from demos/hello2.lang
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

string msg;
void compiler_main();
string get_message();
void free(void* ptr);
void* malloc(u64 size);
void printf(u8* fmt, ...);
void memcpy(void* dest, void* src, u64 n);
string substring(string s, u64 up_to);



void __compiler_main() {
__global_initializers();
printf(msg.data);
}

string get_message() {
return __make_string("Hello, world\n", 16);
}

string substring(string s, u64 up_to) {
string sub;
sub.data = malloc(up_to+1);
memcpy(sub.data, s.data, up_to);
sub.data[up_to] = 0;
sub.length = up_to;
return sub;
}

void __global_initializers() {
msg = get_message();
}
int main(int __argcount, char *__args[]) {
__compiler_main();
}

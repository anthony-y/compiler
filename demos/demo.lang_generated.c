// This code was generated by a compiler from demos/demo.lang
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

void compiler_main();
void defer_test();
string scuffed_substring(string s, int up_to);
string to_string(u64 i);
void print(string s);
void memcpy(void* dest, void* src, u64 n);
void* malloc(u64 _);
void free(void* _);
void puts(u8* s);

void __compiler_main() {
    __global_initializers();
    string msg = __make_string("Hello, world!", 13);
    string length_string = to_string(msg.length);
    string substring = scuffed_substring(msg, 5);
    ;
    defer_test();
    print(msg);
    print(substring);
    print(length_string);
    free(substring.data);
}
void defer_test() {
    ;
    ;
    ;
    if (true) {
            print(__make_string("3", 1));
    print(__make_string("2", 1));
    print(__make_string("1", 1));
return;
    };
    print(__make_string("ay", 2));
    print(__make_string("3", 1));
    print(__make_string("2", 1));
    print(__make_string("1", 1));
}
string scuffed_substring(string s, int up_to) {
    string sub;
    u64 up_to_big = (cast(u64)up_to);
    sub.data = malloc((cast(u64)(up_to_big+1)));
    memcpy(sub.data, s.data, up_to_big);
    sub.data[up_to] = 0;
    sub.length = up_to_big;
    return sub;
}
string to_string(u64 i) {
    if (i==0) {
return __make_string("0", 1);
    };
    if (i==1) {
return __make_string("1", 1);
    };
    if (i==2) {
return __make_string("2", 1);
    };
    if (i==3) {
return __make_string("3", 1);
    };
    if (i==4) {
return __make_string("4", 1);
    };
    if (i==5) {
return __make_string("5", 1);
    };
    if (i==6) {
return __make_string("6", 1);
    };
    if (i==7) {
return __make_string("7", 1);
    };
    if (i==8) {
return __make_string("8", 1);
    };
    if (i==9) {
return __make_string("9", 1);
    };
    if (i==10) {
return __make_string("10", 2);
    };
    if (i==11) {
return __make_string("11", 2);
    };
    if (i==12) {
return __make_string("12", 2);
    };
    if (i==13) {
return __make_string("13", 2);
    };
    if (i==14) {
return __make_string("14", 2);
    };
    if (i==15) {
return __make_string("15", 2);
    };
    return __make_string("(bad)", 5);
}
void print(string s) {
    puts(s.data);
}

void __global_initializers() {
}
int main(int __argcount, char *__args[]) {
    __compiler_main();
}

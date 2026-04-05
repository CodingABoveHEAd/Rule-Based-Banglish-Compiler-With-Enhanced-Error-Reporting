#include <stdio.h>
#include <stdlib.h>

int add(int x, int y);
void hello();

int add(int x, int y)
{
    return (x + y);
}

void hello()
{
    printf("%s\n", "hello world");
    return;
}

int main(void)
{
    freopen("NUL", "r", stdin);
    int a = 10;
    double b = 3.1400000000000001;
    int flag = 1;
    int done = 0;
    const int MAX = 100;
    if (((a > 0) && (flag == 1)))
    {
        printf("%s\n", "jodi block");
    }
    else
    {
        printf("%s\n", "nahole block");
    }
    while ((a > 0))
    {
        a = (a - 1);
    }
    for (int i = 0; (i < 3); i++)
    {
        printf("%d\n", (int)i);
        break;
    }
    do
    {
        a = (a + 1);
    }
    while ((a < 5));
    switch (a) {
        case 1:
            printf("%s\n", "one");
            break;
        case 2:
            printf("%s\n", "two");
            break;
        default:
            printf("%s\n", "other");
    }
    a += 2;
    a -= 1;
    a *= 3;
    a /= 2;
    a++;
    a--;
    scanf("%d", &a);
    printf("%g\n", (double)(a + b));
    printf("%c\n", 'X');
    return 0;
}

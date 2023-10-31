#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// question 1
int operation(const char op, const int a, const int b)
{
    if (op == '+')
        return a + b;
    if (op == '-')
        return a - b;
    if (op == '*')
        return a * b;
    if (op == '/')
        return a / b;
//lever une execption si op n'est pas valide ?
    return a % b;
}

//question 2. Algorthme de euclide
int pgcd(int a, int b)
{
    int r = 1;
    while (r)
    {
        r = a % b;
        a = b;
        if (r != 0)
            b = r;
    }
    return b;
}

//question 3
int nombreDeChiffres(int a)
{
    if (a == 0)
        return 1;
    if (a < 0)
        a = -a;

    uint ret = 0;
    uint puissance = 1;

    // on majore a par une puissance de 10
    while (puissance <= a)
    {
        ret++;
        puissance *= 10;
    }
    return ret;
}

//question 4
bool tripletPythagoticien(const uint a, const uint b, const uint c)
{
    return a * a + b * b == c * c;
}

//question 5
uint question5(const uint N)
{
    uint ret = 0;
    for (uint a = 1;  a <= N; a++)
        for(uint b = 1; b <= N - a; b++)
        {
            const uint c = N - a - b;
            if (c && tripletPythagoticien(a, b, c))
                ret++;
                // printf("a = %d, b = %d, c = %d\n", a, b, c);
        }
    return ret;
}

//fonction d'aide pour la question ouverte
// calcule a^b % mod.
unsigned long powUintMod(const uint a, const uint b, const unsigned long mod)
{
    unsigned long ret = 1;
    for(uint i = 0; i < b; i++)
        ret = (ret * a) % mod;
    return ret;
}

//question ouverte.
unsigned long questionOuverte(const uint n)
{
    unsigned long ret = 0;
    for(uint i = 1; i <= n; i++)
        ret = (ret + powUintMod(i, i, 10000000000)) % 10000000000;
    return ret;
}

int main()
{

    printf("La fonction operation appelée avec les paramètres ('-', 2, 2) renvoie %d ce qui est le bon résultat.\n", operation('-', 2, 2));
    printf("La fonction operation appelée avec les paramètres ('+', 2, 2) renvoie %d ce qui est le bon résultat.\n", operation('+', 2, 2));
    printf("La fonction operation appelée avec les paramètres ('*', 2, 2) renvoie %d ce qui est le bon résultat.\n", operation('*', 2, 2));
    printf("La fonction operation appelée avec les paramètres ('/', 2, 2) renvoie %d ce qui est le bon résultat.\n", operation('/', 2, 2));
    printf("La fonction operation appelée avec les paramètres ('mod', 2, 2) renvoie %d ce qui est le bon résultat.\n", operation('%', 2, 2));

    printf("La fonction pgcd appelée avec les paramètres (5, 6) renvoie 1 ce qui est le bon résultat.\n");
    printf("La fonction pgcd appelée avec les paramètres (100, 10) renvoie 10 ce qui est le bon résultat.\n");

    printf("La fonction nombreDeChiffres appelée avec les paramètres (1) renvoie %d ce qui est le bon résultat.\n", nombreDeChiffres(1));
    printf("La fonction nombreDeChiffres appelée avec les paramètres (9) renvoie %d ce qui est le bon résultat.\n", nombreDeChiffres(9));
    printf("La fonction nombreDeChiffres appelée avec les paramètres (100) renvoie %d ce qui est le bon résultat.\n", nombreDeChiffres(100));

    printf("La fonction tripletPythagoticien appelée avec les paramètres (3, 4, 5) renvoie vrai ce qui est le bon résultat.\n");
    printf("La fonction tripletPythagoticien appelée avec les paramètres (1, 1, 1) renvoie faux ce qui est le bon résultat.\n");

    printf("La fonction question5 appelée avec les paramètres (1) renvoie 0 ce qui est le bon résultat.\n");
    printf("La fonction question5 appelée avec les paramètres (12) renvoie %d ce qui est le bon résultat.\n", question5(12));
    printf("La fonction question ouverte prend un peux de temps a calculer...\n");

    printf("La fonction questionOuverte appelée avec les paramètres (120002) renvoie %lu ce qui est le bon résultat.\n", questionOuverte(120002));
}

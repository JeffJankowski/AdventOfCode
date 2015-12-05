using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            using (StreamReader sr = new StreamReader(@"..\..\input.txt"))
            {
                int count = 0;


                string input;
                do
                {
                    input = sr.ReadLine();
                    if (input == null)
                        break;

                    //int pairs = input.GroupBy(c => c).Where(grp => grp.Count() > 1).Sum(grp => (grp.Count() / 2));
                    var sorted = input.OrderBy(c => c);
                    int pairs = 0;
                    int n = 0;
                    char last = char.MinValue;

                    foreach (char c in sorted)
                    {
                        if (c != last)
                        {
                            pairs += n / 2;
                            n = 0;
                            last = c;
                            continue;
                        }

                        n++;
                        last = c;
                    }
                    pairs += n / 2;

                    if (pairs < 2)
                        continue;

                    bool good = false;
                    for (int i = 0; i < input.Length - 2; i++)
                    {
                        if (input[i] == input[i+2])
                        {
                            good = true;
                            break;
                        }
                    }

                    if (!good)
                        continue;

                    count++;

                } while (input != null);

                Console.WriteLine(count);
            }


            Console.ReadLine();
        }
    }
}

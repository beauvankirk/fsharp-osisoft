using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ExtensionMethods
{
    //public interface HasAsEnumerable<E>
    //{
    //    static IEnumerable<E> AsEnumerable();
    //}
    public static class ExtensionMethods
    {
        /// <summary>
        /// Returns the input typed as a generic IEnumerable of the groups
        /// </summary>
        /// <param name="m"></param>
        /// <returns></returns>
        public static IEnumerable<PBObjLib.Display> AsEnumerable(this PBObjLib.Displays gc)
        {
            foreach (PBObjLib.Display g in gc)
            {
                yield return g;
            }
        }
        public static IEnumerable<PBObjLib.ProcBook> AsEnumerable(this PBObjLib.ProcBooks gc)
        {
            foreach (PBObjLib.ProcBook g in gc)
            {
                yield return g;
            }
        }
        public static IEnumerable<PBObjLib.Symbol> AsEnumerable(this PBObjLib.Symbols gc)
        {
            foreach (PBObjLib.Symbol g in gc)
            {
                yield return g;
            }
        }

        public static IEnumerable<System.Text.RegularExpressions.Group> AsEnumerable(this System.Text.RegularExpressions.GroupCollection gc)
        {
            foreach (System.Text.RegularExpressions.Group g in gc)
            {
                yield return g;
            }
        }
        /// <summary>
        /// Returns the input typed as a generic IEnumerable of the matches
        /// </summary>
        /// <param name="mc"></param>
        /// <returns></returns>
        public static IEnumerable<System.Text.RegularExpressions.Match> AsEnumerable(this System.Text.RegularExpressions.MatchCollection mc)
        {
            foreach (System.Text.RegularExpressions.Match m in mc)
            {
                yield return m;
            }
        }
    }
}

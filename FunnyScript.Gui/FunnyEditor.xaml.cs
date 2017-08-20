using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Reflection;

namespace FunnyScript.Gui
{
  /// <summary>
  /// FunnyEditor.xaml の相互作用ロジック
  /// </summary>
  public partial class FunnyEditor : UserControl
  {
    Script.Env env = Script.Env.Default;
    Result<AST.Trace<AST.Expression>, string> expr;

    public FunnyEditor()
    {
      InitializeComponent();
    }

    public void LoadAssembly( Assembly asm )
    {
      env = env.LoadAssembly( asm );
    }

    public void SetVariable( string name, object obj )
    {
      env = env.Add( name, obj );
    }

    public void Parse()
    {
      expr = Parser.parse( "(noname)", editor.Text );
    }

    public Result<AST.Obj, Script.Error> Run( params Tuple<string, object>[] args )
    {
      var e = env;
      foreach ( var arg in args ) e = e.Add( arg.Item1, arg.Item2 );
      return e.Run( "(noname)", editor.Text );
    }
  }
}

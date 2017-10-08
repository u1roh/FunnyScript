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

namespace FunnyScript.Gui
{
  /// <summary>
  /// MainWindow.xaml の相互作用ロジック
  /// </summary>
  public partial class MainWindow : Window
  {
    public MainWindow()
    {
      InitializeComponent();
      editor.SourceFilePath = System.IO.Path.Combine( System.IO.Path.GetDirectoryName( typeof( MainWindow ).Assembly.Location ), "default.fny" );
			editor.SetFunc("dump", ( string name, object obj ) => { output.Add(name, obj); return obj; });
			editor.SetAction("print", (string text) => output.Add(text));
      this.KeyDown += ( sender, e ) =>
      {
        if ( e.Key == Key.F5 ) this.MenuItem_Click( sender, e );
      };
    }

		private void MenuItem_Click( object sender, RoutedEventArgs e )
		{
			output.Items.Clear();
			var result = editor.Run();
			var text = Script.getResultString(result);
			var obj = result.IsOk ?
				((Result<object, Script.Error>.Ok)result).Item :
				((Result<object, Script.Error>.Error)result).Item;
			int endpos = text.IndexOf('\n');
			output.Items.Add(new ObjListView.Item(obj, endpos == -1 ? text : text.Substring(0, endpos)));
			Console.WriteLine("{0}", text);
		}
  }
}

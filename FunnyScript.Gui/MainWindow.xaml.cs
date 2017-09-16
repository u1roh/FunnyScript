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
			editor.SetVariable("outputWindow", output);
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
			int endpos = text.IndexOf('\n');
			output.Items.Add(new ObjListView.Item(result, endpos == -1 ? text : text.Substring(0, endpos)));
			Console.WriteLine("{0}", text);
		}
  }
}

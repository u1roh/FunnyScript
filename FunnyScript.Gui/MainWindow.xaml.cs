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
using Microsoft.Win32;

namespace FunnyScript.Gui
{
  /// <summary>
  /// MainWindow.xaml の相互作用ロジック
  /// </summary>
  public partial class MainWindow : Window
  {
    static readonly string DefaultFolder = System.IO.Path.GetDirectoryName(typeof(MainWindow).Assembly.Location);

    public MainWindow()
    {
      InitializeComponent();
      editor.SourceFilePath = System.IO.Path.Combine( DefaultFolder, "default.fny" );
			editor.SetFunc("dump", ( string name, object obj ) => { output.Add(name, obj); return obj; });
			editor.SetAction("print", (string text) => output.Add(text));
      this.KeyDown += ( sender, e ) =>
      {
        if ( e.Key == Key.F5 ) this.MenuItem_Run_Click( sender, e );
      };
    }

		private void MenuItem_Run_Click( object sender, RoutedEventArgs e )
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

    private void MenuItem_Open_Click( object sender, RoutedEventArgs e )
    {
      var dialog = new OpenFileDialog();
      dialog.Filter = "Funny Script (*.fny)|*.fny";
      dialog.InitialDirectory = DefaultFolder;
      var result = dialog.ShowDialog(this);
      if (result.HasValue && result.Value) editor.Load(dialog.FileName);
    }

    private void MenuItem_Save_Click( object sender, RoutedEventArgs e )
    {
      editor.Save(editor.SourceFilePath);
    }

    private void MenuItem_SaveAs_Click( object sender, RoutedEventArgs e )
    {
      var dialog = new SaveFileDialog();
      dialog.Filter = "Funny Script (*.fny)|*.fny";
      dialog.InitialDirectory = DefaultFolder;
      var result = dialog.ShowDialog(this);
      if (result.HasValue && result.Value) editor.Save(dialog.FileName);
    }
  }
}

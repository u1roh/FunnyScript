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
using System.IO;
using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Highlighting.Xshd;

namespace FunnyScript.Gui
{
  /// <summary>
  /// FunnyEditor.xaml の相互作用ロジック
  /// </summary>
  public partial class FunnyEditor : UserControl
  {
    public static readonly DependencyProperty SourceFilePathProperty =
      DependencyProperty.Register( "SourceFilePath", typeof( string ), typeof( FunnyEditor ),
        new PropertyMetadata( null, ( self, e ) =>
        {
          ( (FunnyEditor)self ).Save( e.OldValue as string );
          ( (FunnyEditor)self ).Load( e.NewValue as string );
        } ) );

    Script.Env env = Script.Env.Default;
    Result<AST.Trace<AST.Expression>, string> expr;

    public FunnyEditor()
    {
      InitializeComponent();
      using ( var stream = typeof( FunnyEditor ).Assembly.GetManifestResourceStream( "FunnyScript.Gui.FunnyScriptSyntax.xshd" ) ) {
        using ( var reader = System.Xml.XmlReader.Create( stream ) ) {
          editor.SyntaxHighlighting = HighlightingLoader.Load( reader, ICSharpCode.AvalonEdit.Highlighting.HighlightingManager.Instance );
        }
      }
    }

    public string SourceFilePath
    {
      get { return this.GetValue( SourceFilePathProperty ) as string; }
      set { this.SetValue( SourceFilePathProperty, value ); }
    }

    public void Load( string path )
    {
      if ( path != null && File.Exists( path ) ) {
        try {
          editor.Load( path );
        }
        catch ( Exception e ) {
          Console.WriteLine( e );
        }
      }
    }

    public void Save( string path )
    {
      if ( path != null ) {
        try {
          editor.Save( path );
        }
        catch ( Exception e ) {
          Console.WriteLine( e );
        }
      }
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
      if ( expr.IsOk ) this.Save( this.SourceFilePath );
    }

    public Result<AST.Obj, Script.Error> Run( params Tuple<string, object>[] args )
    {
      var e = env;
      foreach ( var arg in args ) e = e.Add( arg.Item1, arg.Item2 );
      var result = e.Run( "(noname)", editor.Text );
      if ( result.IsOk ) this.Save( this.SourceFilePath );
      return result;
    }
  }
}

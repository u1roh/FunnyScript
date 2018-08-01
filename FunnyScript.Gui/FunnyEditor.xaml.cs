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
using ICSharpCode.AvalonEdit.CodeCompletion;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Editing;

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

    Result<AST.Expr, string> expr;
    CompletionWindow completionWindow = null;
    bool keywordCompletion = true;

    class FunnyCompletionData : ICompletionData
    {
      public FunnyCompletionData( string text )
      {
        this.Text = text;
      }

      public string Text { get; } = "";

      public object Content { get { return this.Text; } }

      public object Description { get { return "(no description)"; } }

      public ImageSource Image { get; } = null;

      public double Priority { get; } = 0;

      public void Complete( TextArea textArea, ISegment completionSegment, EventArgs insertionRequestEventArgs )
      {
        textArea.Document.Replace( completionSegment, this.Text );
      }
    }

    public FunnyEditor()
    {
      InitializeComponent();
      using ( var stream = typeof( FunnyEditor ).Assembly.GetManifestResourceStream( "FunnyScript.Gui.FunnyScriptSyntax.xshd" ) ) {
        using ( var reader = System.Xml.XmlReader.Create( stream ) ) {
          editor.SyntaxHighlighting = HighlightingLoader.Load( reader, ICSharpCode.AvalonEdit.Highlighting.HighlightingManager.Instance );
        }
      }

      // コード補完のテスト
      editor.TextArea.TextEntered += ( sender, e ) =>
      {
        if ( char.IsWhiteSpace( e.Text[0] ) || char.IsSymbol( e.Text[0] ) ) {
          keywordCompletion = true;
        }
        else {
          var candidates = new List<string>();
          if ( e.Text == "." ) {
            //candidates.AddRange( new[]
            //{
            //  "Item1",
            //  "Item2",
            //  "Item3",
            //} );
          }
          else if( keywordCompletion ) {
            //switch ( e.Text[0] ) {
            //  case 'o': candidates.Add( "open" ); break;
            //  case 'd': candidates.Add( "do" ); break;
            //}
          }
          if ( candidates.Count > 0 ) {
            completionWindow = new CompletionWindow( editor.TextArea );
            foreach ( var word in candidates ) completionWindow.CompletionList.CompletionData.Add( new FunnyCompletionData( word ) );
            completionWindow.Show();
            completionWindow.Closed += delegate { completionWindow = null; };
          }
          keywordCompletion = false;
        }
      };
      editor.TextArea.TextEntering += ( sender, e ) =>
      {
        if ( e.Text.Length > 0 && completionWindow != null ) {
          if ( !char.IsLetterOrDigit( e.Text[0] ) ) {
            // Whenever a non-letter is typed while the completion window is open,
            // insert the currently selected element.
            completionWindow.CompletionList.RequestInsertion( e );
          }
        }
        // Do not set e.Handled=true.
        // We still want to insert the character that was typed.
      };
    }

    public string SourceFilePath
    {
      get { return this.GetValue( SourceFilePathProperty ) as string; }
      set { this.SetValue( SourceFilePathProperty, value ); }
    }

    public string Text { get { return editor.Text; } set { editor.Text = value; } }

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

    public void Parse()
    {
      expr = Parser.parse( "(noname)", editor.Text );
      if ( expr.IsOk ) this.Save( this.SourceFilePath );
    }
  }
}

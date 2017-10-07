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
using System.Collections.ObjectModel;

namespace FunnyScript.Gui
{
	/// <summary>
	/// ObjListView.xaml の相互作用ロジック
	/// </summary>
	public partial class ObjListView : UserControl
	{
		public class Item
		{
			public object Obj { get; }
			public string Text { get; }
			public Item( object obj, string text ) { Obj = obj; Text = text; }
			public override string ToString() { return this.Text; }
		}

		class ViewModel
		{
			public ObservableCollection<Item> Items { get; } = new ObservableCollection<Item>();
			public ObservableCollection<Inspection.Item> InspectionItems { get; } = new ObservableCollection<Inspection.Item>();

			Item selected;

			public Item SelectedItem
			{
				get { return selected; }
				set
				{
					if (value != selected)
					{
						selected = value;
						this.InspectionItems.Clear();
						if (value != null)
						{
							foreach(var item in Inspection.getItems(value.Obj)) this.InspectionItems.Add(item);
						}
					}
				}
			}
		}

		public ObjListView()
		{
			InitializeComponent();
			this.DataContext = new ViewModel();
		}

		static string DefaultFormatter( string name, object obj )
		{
			if (name == null) name = "";
			return obj == null ? name + " = null" : string.Format("{0} : {1} = {2}", name, obj.GetType().Name, obj);
		}

		public Func<string, object, string> Formatter { get; set; } = DefaultFormatter;

		public ObservableCollection<Item> Items
		{
			get { return ((ViewModel)this.DataContext).Items; }
		}

		public void Add( string name, object obj )
		{
			this.Items.Add(new Item(obj, this.Formatter(name, obj)));
		}

		public void Add( string text )
		{
			this.Items.Add(new Item(null, text));
		}
	}
}

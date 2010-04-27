package org.sc.prefuse;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import prefuse.Constants;
import prefuse.Display;
import prefuse.Visualization;
import prefuse.action.Action;
import prefuse.action.ActionList;
import prefuse.action.ItemAction;
import prefuse.action.RepaintAction;
import prefuse.action.animate.ColorAnimator;
import prefuse.action.animate.LocationAnimator;
import prefuse.action.animate.QualityControlAnimator;
import prefuse.action.animate.VisibilityAnimator;
import prefuse.action.assignment.ColorAction;
import prefuse.action.assignment.FontAction;
import prefuse.action.filter.FisheyeTreeFilter;
import prefuse.action.layout.CollapsedSubtreeLayout;
import prefuse.action.layout.graph.NodeLinkTreeLayout;
import prefuse.action.layout.graph.TreeLayout;
import prefuse.activity.SlowInSlowOutPacer;
import prefuse.controls.ControlAdapter;
import prefuse.controls.FocusControl;
import prefuse.controls.PanControl;
import prefuse.controls.WheelZoomControl;
import prefuse.controls.ZoomControl;
import prefuse.controls.ZoomToFitControl;
import prefuse.controls.ToolTipControl;
import prefuse.data.Tree;
import prefuse.data.Tuple;
import prefuse.data.event.TupleSetListener;
import prefuse.data.io.TreeMLReader;
import prefuse.data.search.PrefixSearchTupleSet;
import prefuse.data.tuple.TupleSet;
import prefuse.render.DefaultRendererFactory;
import prefuse.render.EdgeRenderer;
import prefuse.render.AbstractShapeRenderer;
import prefuse.render.LabelRenderer;
import prefuse.util.ColorLib;
import prefuse.util.FontLib;
import prefuse.util.ui.JFastLabel;
import prefuse.util.ui.JSearchPanel;
import prefuse.util.ui.JValueSlider;
import prefuse.visual.VisualItem;
import prefuse.visual.expression.InGroupPredicate;
import prefuse.visual.sort.TreeDepthItemSorter;
import prefuse.util.GraphicsLib;
import prefuse.util.display.DisplayLib;
import java.awt.geom.Rectangle2D;


/**
 * Demonstration of a node-link tree viewer
 *
 * @version 1.0
 * @author <a href="http://jheer.org">jeffrey heer</a>
 */
public class TreeView extends Display {

    public static final String TREE_CHI = "file:///Users/tdanford/Software/prefuse-beta/data/chi-ontology.xml";
    
    private static final String tree = "tree";
    private static final String treeNodes = "tree.nodes";
    private static final String treeEdges = "tree.edges";
    
    private LabelRenderer m_nodeRenderer;
    private EdgeRenderer m_edgeRenderer;
    
    private String m_label = "label";
    private int m_orientation = Constants.ORIENT_LEFT_RIGHT;

    private FisheyeTreeFilter fisheyeFilter;
    private int fisheyeDistance = 2;
    private JValueSlider fisheyeSlider;
    
    private static String m_detail_label = "entity";
    private static String m_tooltip_label = "tooltip";
    
	private long m_duration = 2000;
	private int m_margin = 50;
	private String m_group = Visualization.ALL_ITEMS;
    
    public TreeView(Tree t, String label,int initial_depth) {
        super(new Visualization());
        m_label = label;

        m_vis.add(tree, t);
        
        m_nodeRenderer = new LabelRenderer(m_label);
        m_nodeRenderer.setRenderType(AbstractShapeRenderer.RENDER_TYPE_FILL);
        m_nodeRenderer.setHorizontalAlignment(Constants.LEFT);
        m_nodeRenderer.setRoundedCorner(8,8);
        m_edgeRenderer = new EdgeRenderer(Constants.EDGE_TYPE_CURVE);
        
        DefaultRendererFactory rf = new DefaultRendererFactory(m_nodeRenderer);
        rf.add(new InGroupPredicate(treeEdges), m_edgeRenderer);
        m_vis.setRendererFactory(rf);
               
        // colors
        ItemAction nodeColor = new NodeColorAction(treeNodes);
        ItemAction textColor = new ColorAction(treeNodes,
                VisualItem.TEXTCOLOR, ColorLib.rgb(0,0,0));
        m_vis.putAction("textColor", textColor);
        
        ItemAction edgeColor = new ColorAction(treeEdges,
                VisualItem.STROKECOLOR, ColorLib.rgb(200,200,200));
        
        // quick repaint
        ActionList repaint = new ActionList();
        repaint.add(nodeColor);
        repaint.add(new RepaintAction());
        m_vis.putAction("repaint", repaint);
        
        // full paint
        ActionList fullPaint = new ActionList();
        fullPaint.add(nodeColor);
        m_vis.putAction("fullPaint", fullPaint);
        
        // animate paint change
        ActionList animatePaint = new ActionList(400);
        animatePaint.add(new ColorAnimator(treeNodes));
        animatePaint.add(new RepaintAction());
        m_vis.putAction("animatePaint", animatePaint);
        
        // create the tree layout action
        NodeLinkTreeLayout treeLayout = new NodeLinkTreeLayout(tree,
                m_orientation, 50, 0, 8);
        treeLayout.setLayoutAnchor(new Point2D.Double(25,300));
        m_vis.putAction("treeLayout", treeLayout);
        
        CollapsedSubtreeLayout subLayout = 
            new CollapsedSubtreeLayout(tree, m_orientation);
        m_vis.putAction("subLayout", subLayout);
        
        ZoomToFitAction zoomer = new ZoomToFitAction(this, tree);
        m_vis.putAction("zoomToFit", zoomer);
        m_vis.alwaysRunAfter("zoomToFit", "repaint");
        
        AutoPanAction autoPan = new AutoPanAction();
        
        // create the filtering and layout
        ActionList filter = new ActionList();
        fisheyeFilter = new FisheyeTreeFilter(tree, initial_depth);
        filter.add(fisheyeFilter);
        filter.add(new FontAction(treeNodes, FontLib.getFont("Tahoma", 16)));
        filter.add(treeLayout);
        filter.add(subLayout);
        filter.add(textColor);
        filter.add(nodeColor);
        filter.add(edgeColor);
        m_vis.putAction("filter", filter);
        
        // animated transition
        ActionList animate = new ActionList(1000);
        animate.setPacingFunction(new SlowInSlowOutPacer());
        animate.add(autoPan);
        animate.add(new QualityControlAnimator());
        animate.add(new VisibilityAnimator(tree));
        animate.add(new LocationAnimator(treeNodes));
        animate.add(new ColorAnimator(treeNodes));
        animate.add(new RepaintAction());
        m_vis.putAction("animate", animate);
        m_vis.alwaysRunAfter("filter", "animate");
        //m_vis.alwaysRunAfter("animate", "zoomToFit");
        
        // create animator for orientation changes
        ActionList orient = new ActionList(2000);
        orient.setPacingFunction(new SlowInSlowOutPacer());
        orient.add(autoPan);
        orient.add(new QualityControlAnimator());
        orient.add(new LocationAnimator(treeNodes));
        orient.add(new RepaintAction());
        m_vis.putAction("orient", orient);
        
        // ------------------------------------------------
        
        // initialize the display
        setSize(1000,700);
        setItemSorter(new TreeDepthItemSorter());
        addControlListener(new ZoomToFitControl());
        addControlListener(new ZoomControl());
        addControlListener(new PanControl());
        addControlListener(new FocusControl(1, "filter"));

        // added this (back?) in --TWD
        addControlListener(new WheelZoomControl());

        registerKeyboardAction(
            new OrientAction(Constants.ORIENT_LEFT_RIGHT),
            "left-to-right", KeyStroke.getKeyStroke("ctrl 1"), WHEN_FOCUSED);
        registerKeyboardAction(
            new OrientAction(Constants.ORIENT_TOP_BOTTOM),
            "top-to-bottom", KeyStroke.getKeyStroke("ctrl 2"), WHEN_FOCUSED);
        registerKeyboardAction(
            new OrientAction(Constants.ORIENT_RIGHT_LEFT),
            "right-to-left", KeyStroke.getKeyStroke("ctrl 3"), WHEN_FOCUSED);
        registerKeyboardAction(
            new OrientAction(Constants.ORIENT_BOTTOM_TOP),
            "bottom-to-top", KeyStroke.getKeyStroke("ctrl 4"), WHEN_FOCUSED);
        
        // ------------------------------------------------
        
        // filter graph and perform layout

        setOrientation(m_orientation);
        m_vis.run("filter");

        TupleSet search = new PrefixSearchTupleSet(); 
        m_vis.addFocusGroup(Visualization.SEARCH_ITEMS, search);

	// want to zoom to fit initially. how?
	//            Visualization vis = tree.getVisualization();
	//	Rectangle2D m_temp = new Rectangle2D.Double();
	//		getItemBounds(m_temp);
		//		GraphicsLib.expand(m_temp, m_margin + (int)(1/getScale()));
	//		DisplayLib.fitViewToBounds(this, m_temp, m_duration);

	//        m_vis.run("treelayout");

        search.addTupleSetListener(new TupleSetListener() {
            public void tupleSetChanged(TupleSet t, Tuple[] add, Tuple[] rem) {
                m_vis.cancel("animatePaint");
                m_vis.run("fullPaint");
                m_vis.run("animatePaint");
            }
        });
    }
    
    // ------------------------------------------------------------------------
    
    public void setOrientation(int orientation) {
        NodeLinkTreeLayout rtl 
            = (NodeLinkTreeLayout)m_vis.getAction("treeLayout");
        CollapsedSubtreeLayout stl
            = (CollapsedSubtreeLayout)m_vis.getAction("subLayout");
        switch ( orientation ) {
        case Constants.ORIENT_LEFT_RIGHT:
            m_nodeRenderer.setHorizontalAlignment(Constants.LEFT);
            m_edgeRenderer.setHorizontalAlignment1(Constants.RIGHT);
            m_edgeRenderer.setHorizontalAlignment2(Constants.LEFT);
            m_edgeRenderer.setVerticalAlignment1(Constants.CENTER);
            m_edgeRenderer.setVerticalAlignment2(Constants.CENTER);
            break;
        case Constants.ORIENT_RIGHT_LEFT:
            m_nodeRenderer.setHorizontalAlignment(Constants.RIGHT);
            m_edgeRenderer.setHorizontalAlignment1(Constants.LEFT);
            m_edgeRenderer.setHorizontalAlignment2(Constants.RIGHT);
            m_edgeRenderer.setVerticalAlignment1(Constants.CENTER);
            m_edgeRenderer.setVerticalAlignment2(Constants.CENTER);
            break;
        case Constants.ORIENT_TOP_BOTTOM:
            m_nodeRenderer.setHorizontalAlignment(Constants.CENTER);
            m_edgeRenderer.setHorizontalAlignment1(Constants.CENTER);
            m_edgeRenderer.setHorizontalAlignment2(Constants.CENTER);
            m_edgeRenderer.setVerticalAlignment1(Constants.BOTTOM);
            m_edgeRenderer.setVerticalAlignment2(Constants.TOP);
            break;
        case Constants.ORIENT_BOTTOM_TOP:
            m_nodeRenderer.setHorizontalAlignment(Constants.CENTER);
            m_edgeRenderer.setHorizontalAlignment1(Constants.CENTER);
            m_edgeRenderer.setHorizontalAlignment2(Constants.CENTER);
            m_edgeRenderer.setVerticalAlignment1(Constants.TOP);
            m_edgeRenderer.setVerticalAlignment2(Constants.BOTTOM);
            break;
        default:
            throw new IllegalArgumentException(
                "Unrecognized orientation value: "+orientation);
        }
        m_orientation = orientation;
        rtl.setOrientation(orientation);
        stl.setOrientation(orientation);
    }
    
    public int getOrientation() {
        return m_orientation;
    }
    
    // ------------------------------------------------------------------------
    
    public static void main(String argv[]) {
        String infile = TREE_CHI;
        String label = "name";
        if ( argv.length > 1 ) {
            infile = argv[0];
            label = argv[1];
        }
        JComponent treeview = demo(infile, label,"2","10");
        
        JFrame frame = new JFrame("p r e f u s e  |  t r e e v i e w");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setContentPane(treeview);
        frame.pack();
        frame.setVisible(true);
    }
    
    public static JComponent demo() {
        return demo(TREE_CHI, "name","2","10");
    }
    
    public static JComponent demo(String datafile, final String label,String initial_depth, String maximum_depth) {
        Color BACKGROUND = Color.WHITE;
        Color FOREGROUND = Color.BLACK;
        
        Tree t = null;
        try {
            t = (Tree)new TreeMLReader().readGraph(new java.net.URL(datafile));
        } catch ( Exception e ) {
        	e.printStackTrace();
        	System.exit(1);
        }

        // create a new treemap
        final TreeView tview = new TreeView(t, label,Integer.parseInt(initial_depth));
        tview.setBackground(BACKGROUND);
        tview.setForeground(FOREGROUND);

        // create a search panel for the tree map
        JSearchPanel search = new JSearchPanel(tview.getVisualization(),
        		treeNodes, Visualization.SEARCH_ITEMS, label, true, true);
        search.setShowResultCount(true);
        search.setBorder(BorderFactory.createEmptyBorder(5,5,4,0));
        search.setFont(FontLib.getFont("Tahoma", Font.PLAIN, 11));
        search.setBackground(BACKGROUND);
        search.setForeground(FOREGROUND);

        // The following sets up the slider for the fisheye view depth --TWD
        // create a value slider for the fisheye-filter depth
        tview.fisheyeSlider = new JValueSlider("Depth", 1, Integer.parseInt(maximum_depth), Integer.parseInt(initial_depth));
        tview.fisheyeSlider.setForeground(FOREGROUND);
        tview.fisheyeSlider.setBackground(BACKGROUND);
        tview.fisheyeSlider.addChangeListener(new SliderChangeListener(tview));

        final JFastLabel title = new JFastLabel("                 ");
        title.setPreferredSize(new Dimension(350, 20));
        title.setVerticalAlignment(SwingConstants.BOTTOM);
        title.setBorder(BorderFactory.createEmptyBorder(3,0,0,0));
        title.setFont(FontLib.getFont("Tahoma", Font.PLAIN, 16));
        title.setBackground(BACKGROUND);
        title.setForeground(FOREGROUND);

        tview.addControlListener(new ControlAdapter() {
        	public void itemEntered(VisualItem item, MouseEvent e) {
        		if(item.canGetString(label)) { 
        			title.setText(item.getString(m_detail_label));
        		}
        	}
        	public void itemExited(VisualItem item, MouseEvent e) {
        		title.setText(null);
        	}
        });

        ToolTipControl ttc = new ToolTipControl(m_tooltip_label);
        tview.addControlListener(ttc);

        Box box = new Box(BoxLayout.X_AXIS);
        box.add(Box.createHorizontalStrut(10));
        box.add(title);
        box.add(Box.createHorizontalGlue());
        box.add(search);
        box.add(Box.createHorizontalStrut(3));
        box.setBackground(BACKGROUND);

        JPanel panel = new JPanel(new BorderLayout());
        panel.setBackground(BACKGROUND);
        panel.setForeground(FOREGROUND);
        panel.add(tview, BorderLayout.CENTER);
        panel.add(box, BorderLayout.SOUTH);
        panel.add(tview.fisheyeSlider, BorderLayout.NORTH);
        return panel;
    }
    
    // ------------------------------------------------------------------------

    public static class SliderChangeListener implements ChangeListener {
    	
    	private TreeView view;
    	
    	public SliderChangeListener(TreeView v) { 
    		view = v;
    	}
    	
		public void stateChanged(ChangeEvent e) {
			int d = (Integer)view.fisheyeSlider.getValue();
			if(d != view.fisheyeDistance) { 
				view.fisheyeDistance = d;
				view.fisheyeFilter.setDistance(view.fisheyeDistance);
				System.out.println(String.format("Set Fisheye: %d", view.fisheyeDistance));
				view.m_vis.cancel("animatePaint");
				view.m_vis.run("filter");
	            view.m_vis.run("zoomToFit");

	            view.m_vis.run("fullPaint");
	            //view.m_vis.run("animatePaint");
			}
		} 
    }
    
    public static class ZoomToFitAction extends prefuse.action.Action {
    	
    	private Display display;
    	private String m_group;
		private int m_margin;
		private long m_duration;
    	
    	public ZoomToFitAction(Display d, String g) { 
    		display = d;
    		m_group = g;
    		m_margin = 10;
    		m_duration = 1;
    	}
    	
    	public void run(double frac) {
    		if ( !display.isTranformInProgress() )
    		{
    			Visualization vis = display.getVisualization();
    			Rectangle2D bounds = vis.getBounds(m_group);
    			GraphicsLib.expand(bounds, m_margin + (int)(1/display.getScale()));
    			Point2D pt = DisplayLib.getCentroid(vis.visibleItems());
    			DisplayLib.fitViewToBounds(display, bounds, pt, m_duration);
    		}
    	} 
    }
    
    public class OrientAction extends AbstractAction {
        private int orientation;
        
        public OrientAction(int orientation) {
            this.orientation = orientation;
        }
        public void actionPerformed(ActionEvent evt) {
            setOrientation(orientation);
            getVisualization().cancel("orient");
            getVisualization().run("treeLayout");
            getVisualization().run("orient");
        }
    }
    
    public class AutoPanAction extends Action {
        private Point2D m_start = new Point2D.Double();
        private Point2D m_end   = new Point2D.Double();
        private Point2D m_cur   = new Point2D.Double();
        private int     m_bias  = 150;
        
        public void run(double frac) {
            TupleSet ts = m_vis.getFocusGroup(Visualization.FOCUS_ITEMS);
            if ( ts.getTupleCount() == 0 )
                return;
            
            if ( frac == 0.0 ) {
                int xbias=0, ybias=0;
                switch ( m_orientation ) {
                case Constants.ORIENT_LEFT_RIGHT:
                    xbias = m_bias;
                    break;
                case Constants.ORIENT_RIGHT_LEFT:
                    xbias = -m_bias;
                    break;
                case Constants.ORIENT_TOP_BOTTOM:
                    ybias = m_bias;
                    break;
                case Constants.ORIENT_BOTTOM_TOP:
                    ybias = -m_bias;
                    break;
                }

                VisualItem vi = (VisualItem)ts.tuples().next();
                m_cur.setLocation(getWidth()/2, getHeight()/2);
                getAbsoluteCoordinate(m_cur, m_start);
                m_end.setLocation(vi.getX()+xbias, vi.getY()+ybias);
            } else {
                m_cur.setLocation(m_start.getX() + frac*(m_end.getX()-m_start.getX()),
                                  m_start.getY() + frac*(m_end.getY()-m_start.getY()));
                panToAbs(m_cur);
            }
        }
    }
    
    public static class NodeColorAction extends ColorAction {
        
        public NodeColorAction(String group) {
            super(group, VisualItem.FILLCOLOR);
        }
        
        public int getColor(VisualItem item) {
            if ( m_vis.isInGroup(item, Visualization.SEARCH_ITEMS) )
                return ColorLib.rgb(255,190,190);
            else if ( m_vis.isInGroup(item, Visualization.FOCUS_ITEMS) )
                return ColorLib.rgb(198,229,229);
            else if ( item.getDOI() > -1 )
                return ColorLib.rgb(164,193,193);
            else
                return ColorLib.rgba(255,255,255,0);
        }
        
    } // end of inner class TreeMapColorAction
    
    
} // end of class TreeMap

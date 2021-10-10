package discourje.core.ctl.formulas;

import discourje.core.ctl.Model;
import discourje.core.ctl.State;
import discourje.core.ctl.formulas.temporal.AU;
import discourje.core.lts.Action;
import org.junit.jupiter.api.Test;
import static discourje.core.ctl.Formulas.close;
import static discourje.core.ctl.Formulas.send;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AUTest<S> extends AbstractCtlFormulaTest<S> {

    @Test
    public void testValidOnAllPathsEarlySplit() {
        State<S> s1 = createState(Action.Type.SEND, "a", "b");
        State<S> s2a = createState(Action.Type.SEND, "a", "b");
        State<S> s2b = createState(Action.Type.SEND, "a", "b");
        State<S> s3a = createState(Action.Type.CLOSE, "a", "b");
        State<S> s3b = createState(Action.Type.CLOSE, "a", "b");

        s1.addNextState(s2a);
        s1.addNextState(s2b);
        s2a.addNextState(s3a);
        s2b.addNextState(s3b);

        Model<S> model = createModel(s1, s2a, s2b, s3a, s3b);

        AU au = new AU(send("a", null), close("a", "b"));
        model.calculateLabels(au);

        assertTrue(model.hasLabel(s1, au));
    }

    @Test
    public void testValidOnAllPathsLateSplit() {
        State<S> s1 = createState(Action.Type.SEND, "a", "b");
        State<S> s2 = createState(Action.Type.SEND, "a", "b");
        State<S> s3a = createState(Action.Type.CLOSE, "a", "b");
        State<S> s3b = createState(Action.Type.CLOSE, "a", "b");

        s1.addNextState(s2);
        s2.addNextState(s3a);
        s2.addNextState(s3b);

        Model<S> model = createModel(s1, s2, s3a, s3b);

        AU au = new AU(send("a", null), close("a", "b"));
        model.calculateLabels(au);

        assertTrue(model.hasLabel(s1, au));
    }

    @Test
    public void testValidOnOnePath() {
        State<S> s1 = createState(Action.Type.SEND, "a", "b");
        State<S> s2a = createState(Action.Type.SEND, "a", "b");
        State<S> s2b = createState(Action.Type.SEND, "a", "b");
        State<S> s3a = createState(Action.Type.CLOSE, "a", "b");
        State<S> s3b = createState(Action.Type.CLOSE, "a", "c");

        s1.addNextState(s2a);
        s1.addNextState(s2b);
        s2a.addNextState(s3a);
        s2b.addNextState(s3b);

        Model<S> model = createModel(s1, s2a, s2b, s3a, s3b);

        AU au = new AU(send("a", null), close("a", "b"));
        model.calculateLabels(au);

        assertFalse(model.hasLabel(s1, au));
    }

    @Test
    public void testValidOnNoPath() {
        State<S> s1 = createState(Action.Type.SEND, "a", "b");
        State<S> s2a = createState(Action.Type.SEND, "a", "b");
        State<S> s2b = createState(Action.Type.SEND, "a", "b");
        State<S> s3a = createState(Action.Type.CLOSE, "a", "c");
        State<S> s3b = createState(Action.Type.CLOSE, "a", "c");

        s1.addNextState(s2a);
        s1.addNextState(s2b);
        s2a.addNextState(s3a);
        s2b.addNextState(s3b);

        Model<S> model = createModel(s1, s2a, s2b, s3a, s3b);

        AU au = new AU(send("a", null), close("a", "b"));
        model.calculateLabels(au);

        assertFalse(model.hasLabel(s1, au));
    }
}
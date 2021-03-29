package discourje.core.validation.formulas;

import discourje.core.lts.Action;
import discourje.core.validation.DMState;
import discourje.core.validation.DiscourjeModel;
import org.junit.jupiter.api.Test;
import static discourje.core.validation.formulas.CtlFormulas.close;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AYTest<S> extends AbstractCtlFormulaTest<S> {

    @Test
    public void testSelfAndAllSuccessors() {
        DMState<S> s1 = createState(Action.Type.CLOSE, "a", "b");
        DMState<S> s2a = createState(Action.Type.CLOSE, "a", "b");
        DMState<S> s2b = createState(Action.Type.CLOSE, "a", "b");

        s2a.addNextState(s1);
        s2b.addNextState(s1);

        DiscourjeModel<S> model = createModel(s1, s2a, s2b);

        AY ay = new AY(close("a", "b"));
        ay.label(model);

        assertTrue(s1.hasLabel(model.getLabelIndex(ay)));
    }

    @Test
    public void testNotSelfButAllSuccessors() {
        DMState<S> s1 = createState(Action.Type.SEND, "a", "b");
        DMState<S> s2a = createState(Action.Type.CLOSE, "a", "b");
        DMState<S> s2b = createState(Action.Type.CLOSE, "a", "b");

        s2a.addNextState(s1);
        s2b.addNextState(s1);

        DiscourjeModel<S> model = createModel(s1, s2a, s2b);

        AY ay = new AY(close("a", "b"));
        ay.label(model);

        assertTrue(s1.hasLabel(model.getLabelIndex(ay)));
    }

    @Test
    public void testNotAllSuccessors() {
        DMState<S> s1 = createState(Action.Type.CLOSE, "a", "b");
        DMState<S> s2a = createState(Action.Type.SEND, "a", "b");
        DMState<S> s2b = createState(Action.Type.CLOSE, "a", "b");

        s2a.addNextState(s1);
        s2b.addNextState(s1);

        DiscourjeModel<S> model = createModel(s1, s2a, s2b);

        AY ay = new AY(close("a", "b"));
        ay.label(model);

        assertFalse(s1.hasLabel(model.getLabelIndex(ay)));
    }

    @Test
    public void testNoSuccessors() {
        DMState<S> s1 = createState(Action.Type.CLOSE, "a", "b");
        DMState<S> s2a = createState(Action.Type.SEND, "a", "b");
        DMState<S> s2b = createState(Action.Type.SEND, "a", "b");

        s2a.addNextState(s1);
        s2b.addNextState(s1);

        DiscourjeModel<S> model = createModel(s1, s2a, s2b);

        AY ay = new AY(close("a", "b"));
        ay.label(model);

        assertFalse(s1.hasLabel(model.getLabelIndex(ay)));
    }
}
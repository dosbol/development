package discourje.core.validation.formulas;

import discourje.core.lts.Action;
import discourje.core.validation.DMState;
import discourje.core.validation.DiscourjeModel;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class SelfTest<S> extends AbstractCtlFormulaTest<S> {

    @Test
    public void testSelf() {
        DMState<S> s1 = createState(Action.Type.SYNC, "a", "a");
        DMState<S> s2 = createState(Action.Type.SEND, "a", "a");
        DMState<S> s3 = createState(Action.Type.RECEIVE, "a", "a");
        DMState<S> s4 = createState(Action.Type.CLOSE, "a", "a");
        DMState<S> s5 = createState(Action.Type.SYNC, "a", "b");
        DMState<S> s6 = createState(Action.Type.SYNC, "b", "a");
        DiscourjeModel<S> model = createModel(s1, s2, s3, s4, s5, s6);

        Self self = new Self("a");
        self.label(model);

        assertTrue(s1.hasLabel(model.getLabelIndex(self)));
        assertTrue(s2.hasLabel(model.getLabelIndex(self)));
        assertTrue(s3.hasLabel(model.getLabelIndex(self)));
        assertTrue(s4.hasLabel(model.getLabelIndex(self)));
        assertFalse(s5.hasLabel(model.getLabelIndex(self)));
        assertFalse(s6.hasLabel(model.getLabelIndex(self)));

    }
}
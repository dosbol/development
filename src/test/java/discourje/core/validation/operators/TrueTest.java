package discourje.core.validation.operators;

import discourje.core.lts.Action;
import discourje.core.validation.DMState;
import discourje.core.validation.DiscourjeModel;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class TrueTest<S> extends AbstractOperatorTest<S> {

    @Test
    public void testTrue() {
        DMState<S> s1 = createState(Action.Type.SYNC, "a", "b");
        DiscourjeModel<S> model = createModel(s1);

        Send snd = new Send("a");
        snd.label(model);

        assertTrue(s1.hasLabel(snd));
    }
}
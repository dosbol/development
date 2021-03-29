package discourje.core.validation.formulas;

import discourje.core.lts.Action;
import discourje.core.lts.State;
import discourje.core.validation.DMState;
import discourje.core.validation.DiscourjeModel;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class FirstTest<S> extends AbstractCtlFormulaTest<S> {

    @Test
    public void testFirst() {
        DMState<S> firstState = new DMState<S>(mock(State.class), null);
        DMState<S> secondState = createState(Action.Type.SEND, "a", "b");
        DiscourjeModel<S> model = createModel(firstState, secondState);

        First first = First.INSTANCE;
        first.label(model);

        assertTrue(firstState.hasLabel(model.getLabelIndex(first)));
        assertFalse(secondState.hasLabel(model.getLabelIndex(first)));
    }
}
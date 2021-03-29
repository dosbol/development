package discourje.core.validation.formulas;

import discourje.core.lts.Action;
import discourje.core.validation.DMState;
import discourje.core.validation.DiscourjeModel;
import java.util.Objects;

class Receive implements CtlFormula {
    private final String role;
    private final int hash;

    Receive(String role) {
        this.role = role;
        hash = Objects.hash(this.role);
    }

    @Override
    public void label(DiscourjeModel<?> model) {
        if (!model.isLabelledBy(this)) {
            int labelIndex = model.setLabelledBy(this);
            for (DMState<?> state : model.getStates()) {
                Action action = state.getAction();
                if (action != null &&
                        action.getType() != Action.Type.CLOSE &&
                        role.equals(action.getReceiver())) {
                    state.addLabel(labelIndex);
                }
            }
        }
    }

    public String toString() {
        return "rcv_" + role;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Receive that = (Receive) o;
        return role.equals(that.role);
    }

    @Override
    public int hashCode() {
        return hash;
    }
}

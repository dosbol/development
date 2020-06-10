package discourje.core.graph;

import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Predicate;
import java.util.function.Supplier;

@SuppressWarnings("unused")
public class Vertex<State, Action, Test extends Predicate<Object> & Supplier<Object>> {

    public final Graph<State, Action, Test> graph;
    public final State state;

    private AtomicReference<Edge.Collection<State, Action, Test>> edges = new AtomicReference<>(null);

    public Vertex(Graph<State, Action, Test> graph, State state) {
        this.graph = graph;
        this.state = state;
    }

    //
    // java.lang.Object
    //

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Vertex<?, ?, ?> vertex = (Vertex<?, ?, ?>) o;
        return Objects.equals(state, vertex.state);
    }

    @Override
    public int hashCode() {
        return Objects.hash(state);
    }

    @Override
    public String toString() {
        return Integer.toString(graph.getVertexId(this));
    }

    //
    // Vertex
    //

    public void expand() {
        expandRecursively(1);
    }

    public void expandRecursively() {
        expandRecursively(Integer.MAX_VALUE);
    }

    public void expandRecursively(int depth) {
        if (depth > 0 && edges.get() == null) {
            var source = this;
            var expansion = new Edge.Collection<State, Action, Test>();
            var targetStates = graph.expander.apply(state);
            for (Map.Entry<Edge.Label<Action, Test>, Collection<State>> e : targetStates.entrySet()) {
                var l = e.getKey();
                for (State targetState : e.getValue()) {
                    var target = graph.newOrGetVertex(targetState);
                    expansion.add(new Edge<>(source, target, l));
                }
            }

            edges.compareAndSet(null, expansion);

            if (depth > 1) {
                for (Vertex<?, ?, ?> target : edges.get().getTargets()) {
                    target.expandRecursively(depth - 1);
                }
            }
        }
    }

    public Collection<Edge.Label<Action, Test>> getLabels() {
        return edges.get().getLabels();
    }

    public boolean isExpanded() {
        return edges.get() != null;
    }

    public boolean traverseEventually(Action a, Object o) {
        var todo = new Stack<Vertex<State, Action, Test>>();
        var done = new HashSet<>();

        todo.push(this);
        while (!todo.isEmpty()) {
            var v = todo.pop();
            if (traverseNow(a, o).isEmpty()) {
                done.add(v);
                for (Vertex<State, Action, Test> target : v.edges.get().getTargets()) {
                    if (!done.contains(v)) {
                        todo.push(target);
                    }
                }
            } else {
                return true;
            }
        }
        return false;
    }

    public Collection<Vertex<State, Action, Test>> traverseNow(Edge.Label<Action, Test> l) {
        expand();
        return edges.get().getTargets(l);
    }

    public Collection<Vertex<State, Action, Test>> traverseNow(Action a, Object o) {
        expand();
        return edges.get().getTargets(a, o);
    }

    public int size() {
        return edges.get().size();
    }

    public String toStringDeep() {
        var b = new StringBuilder();

        if (isExpanded()) {
            for (Edge<State, Action, Test> e : edges.get()) {
                b.append(e);
                b.append(System.lineSeparator());
            }
            if (b.length() > 0) {
                b.deleteCharAt(b.length() - 1);
            }
        } else {
            b.append("*** ").append(this).append(" not yet expanded ***");
        }

        return b.toString();
    }
}

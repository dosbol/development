package discourje.core.graph;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class Graph<State, Action, Test extends Predicate<Object> & Supplier<Object>> {

    public final Function<State, Map<Edge.Label<Action, Test>, Collection<State>>> expander;

    private final Collection<Vertex<State, Action, Test>> roots = new LinkedHashSet<>();
    private final Map<State, Vertex<State, Action, Test>> vertices = new ConcurrentHashMap<>();
    private final Map<Vertex<State, Action, Test>, Integer> vertexIds = new ConcurrentHashMap<>();
    private final AtomicInteger nextVertexId = new AtomicInteger();

    public Graph(Collection<State> initialStates, Function<State, Map<Edge.Label<Action, Test>, Collection<State>>> expander) {
        for (State s : initialStates) {
            roots.add(newOrGetVertex(s));
        }
        this.expander = expander;
    }

    //
    // java.lang.Object
    //

    @Override
    public String toString() {
        if (roots.size() != 1) {
            throw new IllegalStateException();
        }

        var root = roots.iterator().next();
        if (vertexIds.get(root) != 0) {
            throw new IllegalStateException();
        }

        var b = new StringBuilder();

        var nVertices = vertices.size();
        var nEdges = vertices.values()
                .stream()
                .mapToInt(v -> v.isExpanded() ? v.size() : 0)
                .reduce(Integer::sum).getAsInt();
        b.append("des (0,").append(nEdges).append(",").append(nVertices).append(")");

        var sortedVertices = new ArrayList<>(vertices.values());
        sortedVertices.sort(Comparator.comparingInt(vertexIds::get));
        for (Vertex<State, Action, Test> v : sortedVertices) {
            b.append(System.lineSeparator());
            b.append(v.toStringDeep());
        }

        return b.toString();
    }

    //
    // Graph
    //

    public Collection<Vertex<State, Action, Test>> getRoots() {
        return roots;
    }

    public int getVertexId(Vertex<?, ?, ?> v) {
        if (!vertexIds.containsKey(v)) {
            throw new IllegalArgumentException();
        }

        return vertexIds.get(v);
    }

    public Collection<Vertex<State, Action, Test>> getVertices() {
        return vertices.values();
    }

    public Vertex<State, Action, Test> newOrGetVertex(State s) {
        var v = vertices.computeIfAbsent(s, k -> new Vertex<>(this, s));
        vertexIds.computeIfAbsent(v, k -> nextVertexId.getAndIncrement());
        return v;
    }
}

# Build stage
FROM node:22-alpine AS builder

WORKDIR /app

# Copy package files
COPY package.json package-lock.json ./
COPY elm.json elm-tooling.json ./

# Install dependencies using npm ci
# --ignore-scripts prevents elm npm package installation (via vite-plugin-elm-watch -> elm)
# which is not supported on ARM64. elm-tooling handles Elm installation properly.
RUN npm ci --ignore-scripts

# Copy elm-land config, source files, and static assets
COPY elm-land.json ./
COPY src/ ./src/
COPY static/ ./static/

# Install elm tooling and build the application
RUN npx elm-tooling install
RUN npx elm-land build

# Production stage
FROM httpd:alpine

# Copy the built static files from the builder stage
COPY --from=builder /app/dist/ /usr/local/apache2/htdocs/

# Expose port 80
EXPOSE 80

# httpd:alpine runs httpd in the foreground by default
CMD ["httpd", "-D", "FOREGROUND"]
